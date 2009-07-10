package tralesld;

import java.awt.Color;
import java.io.File;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.*;

import tralesld.gui.*;
import tralesld.storage.*;
import tralesld.struct.chart.*;
import tralesld.struct.source.*;
import tralesld.struct.trace.*;
import tralesld.struct.tree.*;
import tralesld.util.*;
import tralesld.visual.tree.TreeViewNode;
import tralesld.visual.tree.TreeViewPanel;

public class TraleSld
{
    // A special step detail key indicating that the corresponding value is the
    // only one of interest, step detail panel needn't be tabbed
    public static String STEPDETAIL_SINGLE_KEY = "single";

    TraleSldGui gui;

    // database connection
    private Connection connection;
    private File db;

    // current chart model
    public ChartModel curCM;
    
    //map TRALE-internal node IDs to trace nodes
    public DataStore<Integer> idConv;

    // chart is stored in form of chart changes for each trace node
    public DataStore<List<ChartModelChange>> chartChanges;
    public DataStore<List<Integer>> chartDependencies;
    // index chart edges by TRALE numbering
    public DataStore<ChartEdge> chartEdges;
    // associate decision tree IDs with chart edges
    public DataStore<Integer> edgeToNode;
    public DataStore<ChartEdge> nodeToEdge;

    // encode tree structure in second dimension: call tree
    public DataStore<Integer> stepAncestors;
    public DataStore<ArrayList<Integer>> stepChildren;
    public DataStore<Integer> recursionDepths;

    public DataStore<Integer> stepFollowers;
    public DataStore<Integer> stepStatus;
    public DataStore<String> nodeCommands;
    
    //store information whether steps exited or failed deterministically
    public Set<Integer> deterministicallyExited;
    
    // contains source code location for each step
    public DataStore<SourceCodeLocation> sourceLocations;

    // store feature structure representations, bindings etc. here
    public DataStore<Map<String, String>> nodeData;

    // map overview tree nodes via stepIDs to associated edges
    public DataStore<ChartEdge> edgeRegister;

    // build feature structure string currently transmitted in this buffer
    public StringBuilder builder;

    public Tracer tracer;

    public int currentDecisionTreeHead;
    public int currentDecisionTreeNode = 0;

    public int lastActiveID = -1;

    ChartEdge lastEdge;

    ChartModelChange currentLexicalEdge;

    int currentOverviewTreeNode;

    LinkedList<ChartEdge> activeEdgeStack;

    HashSet<ChartEdge> successfulEdges;

    // current signal to prolog
    public char reply = 'n';

    boolean autoCompleteMode;

    // during skip node, the step ID of the skipped step is stored here
    public int skipToStep;

    boolean inSkip;

    public void start()
    {
        System.err.print("Trying to build GUI window... ");
        try
        {
            startDatabase();
            gui = TraleSldGui.createAndShowGUI(this);
            autoCompleteMode = false;
            skipToStep = -1;
            inSkip = false;
            System.err.println("Success.");
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    private void startDatabase() throws ClassNotFoundException, SQLException, IOException
    {
        Class.forName("org.apache.derby.jdbc.EmbeddedDriver");
        db = File.createTempFile("traleslddb", null);
        Utilities.deleteRecursively(db);
        connection = DriverManager.getConnection("jdbc:derby:" + db.getPath() + ";create=true");
        // db.deleteOnExit(); // should work but doesn't
        Statement statement = connection.createStatement();

        try
        {
            statement.executeUpdate("DROP TABLE data");
        }
        catch (SQLException e)
        {
            // ignore - gotta hate Derby for not supporting DROP TABLE IF EXISTS
        }

        statement.executeUpdate("CREATE TABLE data (id BIGINT NOT NULL , value VARCHAR(32) NOT NULL, PRIMARY KEY (id))");
        statement.close();
    }

    public void stop()
    {
        try
        {
            connection.close();
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }

        // Since File.deleteOnExit() doesn't seem to work:
        if (db.exists())
        {
            Utilities.deleteRecursively(db);
        }

        // TODO tell TRALE to abort parsing process, don't exit
        System.exit(0);
    }

    public void initializeParseTrace(String parsedSentenceList)
    {
        System.err.println("Trying to initialize parse trace... ");
        try
        {
            List<String> wordList = PrologUtilities.parsePrologStringList(parsedSentenceList);
            idConv = new DataStore<Integer>();
            idConv.put(0, 0);
            curCM = new ChartModel(wordList);
            chartChanges = new DataStore<List<ChartModelChange>>();
            chartDependencies = new DataStore<List<Integer>>();
            chartEdges = new DataStore<ChartEdge>();
            edgeToNode = new DataStore<Integer>();
            nodeToEdge = new DataStore<ChartEdge>();
            stepAncestors = new DataStore<Integer>();
            stepChildren = new DataStore<ArrayList<Integer>>();
            recursionDepths = new DataStore<Integer>();
            recursionDepths.put(0, 0);
            stepFollowers = new DataStore<Integer>();
            stepStatus = new DataStore<Integer>();
            deterministicallyExited = new HashSet<Integer>();
            List<Integer> nodeToMark = new ArrayList<Integer>();
            gui.dtp.viewExtensionsBeforeMainRendering.add(new CallDimensionViewExtension(stepAncestors, recursionDepths, nodeToMark));
            gui.otp.viewExtensionsAfterMainRendering.add(new NodeMarkingViewExtension(Color.YELLOW));
            gui.dtp.viewExtensionsAfterMainRendering.add(new NodeMarkingViewExtension(Color.YELLOW));
            gui.dtp.viewExtensionsAfterMainRendering.add(new DeterminismViewExtension(deterministicallyExited));
            gui.dtp.setVisibleEdges(false);
            gui.dtp.setNodePositioning(TreeViewPanel.LEFT_ALIGNMENT);
            // gui.dtp.viewExtensionsAfterMainRendering.add(new
            // SelectionAreaExtension());

            sourceLocations = new DataStore<SourceCodeLocation>();
            nodeCommands = new DataStore<String>();
            nodeCommands.put(0, "init");
            nodeData = new DataStore<Map<String, String>>();
            edgeRegister = new DataStore<ChartEdge>();

            builder = new StringBuilder();

            activeEdgeStack = new LinkedList<ChartEdge>();
            successfulEdges = new HashSet<ChartEdge>();

            tracer = new Tracer();
            tracer.overviewTraceView.generateNode(0, "parsing " + wordList);
            tracer.overviewTraceView.rootID = 0;
            currentDecisionTreeHead = 0;
            currentOverviewTreeNode = 0;

            currentLexicalEdge = null;
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    public void registerStepInformation(int stepID, String command)
    {
        System.err.println("Trying to register step information (" + idConv.size() + "," + command + ")... ");
        try
        {
            int internalStepID = idConv.size();
            idConv.put(stepID, internalStepID);
            nodeCommands.put(internalStepID, command);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    public void registerStepSourceCodeLocation(int id, String absolutePath, int lineNumber)
    {
        System.err.println("Trying to register source code location (" + idConv.getData(id) + "," + absolutePath + "," + lineNumber + ")... ");
        sourceLocations.put(idConv.getData(id), new SourceCodeLocation(absolutePath, lineNumber - 1));
        gui.updateSourceDisplay();
    }

    public void registerRuleApplication(int id, int left, int right, String ruleName)
    {
        int internalStepID = idConv.size();
        idConv.put(id, internalStepID);
        System.err.println("Trying to register rule application (" + internalStepID + "," + ruleName + "," + left + "," + right + ")... ");
        try
        {
            nodeCommands.put(internalStepID, "rule(" + ruleName + ")");
            ChartEdge currentEdge = new ChartEdge(left, right, ruleName, ChartEdge.ACTIVE, true);
            edgeToNode.put(currentEdge.id, internalStepID);
            nodeToEdge.put(internalStepID, currentEdge);
            ChartModelChange cmc = new ChartModelChange(1, currentEdge);
            addChartChange(internalStepID, cmc);
            activeEdgeStack.add(0, currentEdge);

            tracer.overviewTraceView.generateNode(internalStepID, ruleName);
            tracer.overviewTraceView.addChild(currentOverviewTreeNode, internalStepID);
            currentOverviewTreeNode = internalStepID;
            stepStatus.put(internalStepID, Step.STATUS_PROGRESS);
            edgeRegister.put(internalStepID, currentEdge);
            lastEdge = currentEdge;

            if (skipToStep == -1)
            {
                gui.updateAllDisplays();
            }
            gui.centerViewsOnCurrentNode();
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    public void registerStepLocation(String callStack)
    {
        System.err.println("Trying to register step location (" + callStack + ")... ");
        try
        {
            List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
            int extStepID = stack.get(0);
            int stepID = idConv.getData(extStepID);
            stepFollowers.put(lastActiveID, stepID);
            lastActiveID = stepID;
            int extAncestorID = stack.get(1);
            int ancestorID = idConv.getData(extAncestorID);
            if (stepChildren.getData(ancestorID) == null)
            {
                stepChildren.put(ancestorID, new ArrayList<Integer>());
            }
            stepChildren.getData(ancestorID).add(stepID);
            stepAncestors.put(stepID, ancestorID);
            recursionDepths.put(stepID, stack.size() - 1);
            tracer.registerStepAsChildOf(currentDecisionTreeNode, stepID, nodeCommands.getData(stepID));
            stepFollowers.put(lastActiveID, stepID);
            currentDecisionTreeNode = stepID;
            gui.selectDecisionTreeNode(stepID);
            if (nodeCommands.getData(stepID).startsWith("rule_close"))
            {
                if (currentLexicalEdge != null)
                {
                    addChartChange(stepID, currentLexicalEdge);
                    currentLexicalEdge = null;
                }
                tracer.overviewTraceView.generateNode(stepID, lastEdge.desc);
                tracer.overviewTraceView.addChild(currentOverviewTreeNode, stepID);
                currentOverviewTreeNode = stepID;
                edgeRegister.put(stepID, lastEdge);
                stepStatus.put(stepID, Step.STATUS_PROGRESS);
                if (skipToStep == -1)
                {
                    gui.updateAllDisplays();
                    gui.selectChartEdge(lastEdge);
                }
                gui.centerViewsOnCurrentNode();
            }
            else if (nodeCommands.getData(stepID).startsWith("rule"))
            {
                if (skipToStep == -1)
                {
                    gui.updateAllDisplays();
                    gui.selectChartEdge(lastEdge);
                }
                gui.centerViewsOnCurrentNode();
            }
            else
            {
                if (skipToStep == -1)
                {
                    gui.updateAllDisplays();
                }
                gui.centerViewsOnCurrentNode();
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    //MAJOR REWRITE NECESSARY! USE convIDs!!!
    public void registerStepRedo(String callStack)
    {
        System.err.println("Trying to register step redo (" + callStack + ")... ");
        try
        {
            List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
            int extStepID = stack.remove(0);
            int lastStepID = idConv.getData(extStepID);
            
            int newStepID = idConv.size();
            idConv.put(extStepID, newStepID);
            nodeCommands.put(newStepID, nodeCommands.getData(lastStepID));
            
            stepFollowers.put(lastActiveID, newStepID);
            lastActiveID = newStepID;
            int extAncestorID = stack.get(1);
            int ancestorID = idConv.getData(extAncestorID);

            stepChildren.getData(ancestorID).add(newStepID);
            stepAncestors.put(newStepID, ancestorID);
            recursionDepths.put(newStepID, stack.size() - 1);
            
            int decisionParentNodeID = tracer.getParent(lastStepID);
            tracer.registerStepAsChildOf(decisionParentNodeID, newStepID, nodeCommands.getData(newStepID));            
            
            gui.nodeColorings.put(newStepID, Color.ORANGE);
            currentDecisionTreeNode = newStepID;
            gui.selectDecisionTreeNode(currentDecisionTreeNode);
            if (skipToStep == -1)
            {
                gui.selectChartEdge(lastEdge);
            }
            gui.centerViewsOnCurrentNode();
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    /**
     * 
     * @param callStack
     * @param deterministic
     *            If {@code true}, this step exited deterministically and is
     *            guaranteed not to be backtracked into.
     */
    public void registerStepExit(String callStack, boolean deterministic)
    {
        System.err.println("Trying to register step exit (" + callStack + ", " + deterministic + ")... ");
        try
        {
            List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
            int extStepID = stack.remove(0);
            int stepID = idConv.getData(extStepID);
            if (deterministic) deterministicallyExited.add(stepID);
            gui.nodeColorings.put(stepID, Color.GREEN);
            stepFollowers.put(lastActiveID, stepID);
            lastActiveID = stepID;
            gui.selectDecisionTreeNode(stepID);
            if (stepID == skipToStep)
            {
                inSkip = false;
                skipToStep = -1;
                reply = 'n';
            }
            if (skipToStep == -1)
            {
                gui.updateAllDisplays();
            }
            if (nodeCommands.getData(stepID).startsWith("unify") || nodeCommands.getData(stepID).startsWith("featval"))
            {
                gui.collapseCallDimension(stepID);
            }
            gui.centerViewsOnCurrentNode();
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    public void registerStepFinished(String callStack)
    {
        System.err.println("Trying to register step finished (" + callStack + ")... ");
        try
        {
            List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
            int extStepID = stack.remove(0);
            int stepID = idConv.getData(extStepID);
            stepFollowers.put(lastActiveID, stepID);
            deterministicallyExited.add(stepID);
            lastActiveID = stepID;
            gui.nodeColorings.put(stepID, Color.CYAN);
            currentDecisionTreeNode = stack.remove(0);
            gui.selectDecisionTreeNode(currentDecisionTreeNode);
            if (nodeCommands.getData(stepID).startsWith("rule_close"))
            {
                stepStatus.put(stepID, Step.STATUS_SUCCESS);
                // move up one level in overview tree
                currentOverviewTreeNode = tracer.overviewTraceView.treeNodes.get(currentOverviewTreeNode).getParent();
                lastEdge = edgeRegister.getData(currentOverviewTreeNode);
            }
            if (skipToStep == -1)
            {
                gui.selectChartEdge(lastEdge);
            }
            gui.centerViewsOnCurrentNode();
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    public void registerStepFailure(String callStack)
    {
        System.err.println("Trying to register step failure (" + callStack + ")... ");
        try
        {
            List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
            int extStepID = stack.remove(0);
            int stepID = idConv.getData(extStepID);
            stepFollowers.put(lastActiveID, stepID);
            deterministicallyExited.add(stepID);
            lastActiveID = stepID;
            String command = nodeCommands.getData(stepID);
            // need to handle bug: step failure is called even if edge was successful
            if (command.startsWith("rule("))
            {
                ChartEdge currentEdge = activeEdgeStack.remove(0);
                if (successfulEdges.contains(currentEdge))
                {
                    System.err.println("Successful edge! Deleting from chart model...");
                    ChartModelChange toDelete = null;
                    for (ChartModelChange cmc : chartChanges.getData(stepID))
                    {
                        if (cmc.edge == currentEdge)
                        {
                            toDelete = cmc;
                            break;
                        }
                    }
                    chartChanges.getData(stepID).remove(toDelete);
                    stepStatus.put(stepID, Step.STATUS_SUCCESS);
                    gui.nodeColorings.put(stepID, Color.GREEN);
                }
                // current rule application failed; adapt chart accordingly
                else
                {
                    System.err.println("Failed edge! Leaving it on the chart as junk...");
                    currentEdge.status = ChartEdge.FAILED;
                    currentEdge.active = false;
                    stepStatus.put(stepID, Step.STATUS_FAILURE);
                    gui.nodeColorings.put(stepID, Color.RED);
                }
                // move up one level in overview tree
                currentOverviewTreeNode = tracer.overviewTraceView.treeNodes.get(currentOverviewTreeNode).getParent();
                lastEdge = edgeRegister.getData(currentOverviewTreeNode);
            }
            else
            {
                gui.nodeColorings.put(stepID, Color.RED);
            }
            currentDecisionTreeNode = idConv.getData(stack.remove(0));
            gui.selectDecisionTreeNode(currentDecisionTreeNode);
            if (stepID == skipToStep)
            {
                inSkip = false;
                skipToStep = -1;
                reply = 'n';
            }
            if (skipToStep == -1)
            {
                gui.selectChartEdge(lastEdge);
                gui.updateAllDisplays();
            }
            gui.centerViewsOnCurrentNode();
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    public void registerChartEdge(int number, int left, int right, String ruleName)
    {
        System.err.println("Trying to register chartEdge (" + number + "," + left + "," + right + "," + ruleName + ")... ");
        try
        {
            String token = "";
            if (ruleName.equals("lexicon"))
            {
                token = " \"" + curCM.words.get(curCM.words.size() - (1 + number)) + "\"";
            }
            lastEdge = new ChartEdge(left, right, number + " " + ruleName + token, ChartEdge.SUCCESSFUL, true);
            chartEdges.put(number, lastEdge);
            edgeToNode.put(lastEdge.id, currentDecisionTreeNode);
            nodeToEdge.put(currentDecisionTreeNode, lastEdge);
            ChartModelChange cmc = new ChartModelChange(1, lastEdge);
            if (ruleName.equals("lexicon"))
            {
                currentLexicalEdge = cmc;
            }
            else
            {
                int dtNode = findRuleAncestor(currentDecisionTreeNode);
                addChartChange(dtNode, cmc);
                if (activeEdgeStack.size() > 0)
                {
                    System.err.println("Marking the following edge as successful: " + activeEdgeStack.get(0));
                    successfulEdges.add(activeEdgeStack.get(0));
                }
                currentDecisionTreeHead = dtNode;
                gui.selectDecisionTreeNode(dtNode);
                if (skipToStep == -1)
                {
                    gui.updateAllDisplays();
                }
                gui.centerViewsOnCurrentNode();
            }
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }
    }

    public void registerEdgeDependency(int motherID, int daughterID)
    {
        registerEdgeDependencyInternal(chartEdges.getData(motherID).id, chartEdges.getData(daughterID).id);
    }

    private void registerEdgeDependencyInternal(int internalMotherID, int internalDaughterID)
    {
        try
        {
            if (chartDependencies.getData(internalMotherID) == null)
            {
                chartDependencies.put(internalMotherID, new ArrayList<Integer>());
            }
            chartDependencies.getData(internalMotherID).add(internalDaughterID);
        }
        catch (Exception e)
        {
            e.printStackTrace();
        }

    }

    public void registerMessageChunk(int stepID, String chunk)
    {
        builder.append(chunk);
    }

    public void registerMessageEnd(int stepID, String type)
    {
        if (nodeData.getData(stepID) == null)
        {
            nodeData.put(stepID, new HashMap<String, String>());
        }
        nodeData.getData(stepID).put(type, builder.toString());
        builder = new StringBuilder();
    }

    private int findRuleAncestor(int dtNode)
    {
        while (!nodeCommands.getData(dtNode).startsWith("rule(") && !nodeCommands.getData(dtNode).equals("init"))
        {
            dtNode = tracer.getParent(dtNode);
        }
        return dtNode;
    }

    public ArrayList<Integer> getStepChildren(int nodeID)
    {
        ArrayList<Integer> children = stepChildren.getData(nodeID);
        if (children == null)
            return new ArrayList<Integer>();
        return children;
    }

    public char getPressedButton()
    {
        char oldReply = reply;
        if (reply == 'l')
            return 'c';
        if (reply == 's')
        {
            if (currentDecisionTreeNode == skipToStep)
            {
                if (!inSkip)
                {
                    inSkip = true;
                    return 'c';
                }
                else
                {
                    inSkip = false;
                    skipToStep = -1;
                    reply = 'n';
                    return 'n';
                }
            }
            else
            {
                return 'c';
            }
        }
        reply = 'n';
        return oldReply;
    }

    public void addChartChange(int nodeID, ChartModelChange cmc)
    {
        if (chartChanges.getData(nodeID) == null)
        {
            chartChanges.put(nodeID, new ArrayList<ChartModelChange>());
        }
        chartChanges.getData(nodeID).add(cmc);
    }

    public static void main(String[] args) throws InterruptedException
    {
        TraleSld sld = new TraleSld();
        sld.start();
        sld.initializeParseTrace("[it,walks]");
        Thread.sleep(500);
        sld.registerChartEdge(0, 1, 2, "lexicon");
        Thread.sleep(500);
        sld.registerStepInformation(1, "rule_close");
        sld.registerStepLocation("[1,0]");
        sld.registerStepSourceCodeLocation(1, "/home/johannes/.bashrc", 1);
        Thread.sleep(500);
        sld.registerRuleApplication(2, 1, 3, "head_subject");
        sld.registerStepLocation("[2,1,0]");
        sld.registerStepSourceCodeLocation(2, "/home/johannes/.bashrc", 2);
        Thread.sleep(500);
        sld.registerStepInformation(3, "unify");
        sld.registerStepLocation("[3,2,1,0]");
        Thread.sleep(500);
        sld.registerStepInformation(4, "type");
        sld.registerStepLocation("[4,3,2,1,0]");
        Thread.sleep(500);
        sld.registerStepFailure("[4,3,2,1,0]");
        Thread.sleep(500);
        sld.registerStepFailure("[3,2,1,0]");
        Thread.sleep(500);
        sld.registerStepFailure("[2,1,0]");
        Thread.sleep(500);
        sld.registerStepFinished("[1,0]");
        Thread.sleep(500);
        sld.registerChartEdge(1, 0, 1, "lexicon");
        Thread.sleep(500);
        sld.registerStepInformation(5, "rule_close");
        sld.registerStepLocation("[5,4,3,2,1,0]");
        Thread.sleep(500);
        sld.registerRuleApplication(6, 0, 2, "head_subject");
        sld.registerStepLocation("[6,5,4,3,2,1,0]");
        Thread.sleep(500);
        sld.registerEdgeDependency(0, 1);
        // sld.stop();
    }
}
