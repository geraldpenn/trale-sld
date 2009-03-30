package tralesld;

import java.awt.Color;
import java.util.*;

import tralesld.gui.*;
import tralesld.storage.*;
import tralesld.struct.chart.*;
import tralesld.struct.trace.*;
import tralesld.struct.tree.*;
import tralesld.util.*;

public class TraleSld
{
    TraleSldGui gui;
    
    //current chart model
    public ChartModel curCM;
    
    //chart is stored in form of chart changes for each trace node
    public DataStore<List<ChartModelChange>> chartChanges;
    public DataStore<XMLTraceNode> traceNodes;
    public DataStore<Integer> stepStatus;
    public DataStore<String> nodeCommands;
    //map overview tree nodes via stepIDs to associated edges
    public DataStore<ChartEdge> edgeRegister;
    
    public Tracer tracer;
    public XMLTraceNode currentDecisionTreeHead;
    
    int currentDecisionTreeNode = 0;
    ChartEdge lastEdge;
    TreeModelNode currentOverviewTreeNode;
    LinkedList<ChartEdge> activeEdgeStack;
    HashSet<ChartEdge> successfulEdges;
    
    //current signal to prolog
    public char reply = 'n';
    boolean autoCompleteMode;
    
    public TraleSld()
    {
    	System.err.print("Trying to build GUI window... ");
        gui = TraleSldGui.createAndShowGUI(this);
        autoCompleteMode = false;
        System.err.println("Success.");
    }
    
    public void initializeParseTrace(String parsedSentenceList)
    {
    	System.err.println("Trying to initialize parse trace... ");
    	List<String> wordList = PrologUtilities.parsePrologStringList(parsedSentenceList);
    	curCM = new ChartModel(wordList);
    	chartChanges = new DataStore<List<ChartModelChange>>();
    	traceNodes = new DataStore<XMLTraceNode>();
        stepStatus = new DataStore<Integer>();
    	nodeCommands = new DataStore<String>();
    	nodeCommands.put(0, "init");
    	edgeRegister = new DataStore<ChartEdge>();
    	
    	activeEdgeStack = new LinkedList<ChartEdge>();
    	successfulEdges = new HashSet<ChartEdge>();
    	
    	tracer = new Tracer();
    	traceNodes.put(0, tracer.detailedTraceModel.root);
        tracer.overviewTraceModel.addNode(new TreeModelNode(0, "parsing " + wordList));
        tracer.overviewTraceModel.root = 0;
        currentDecisionTreeHead = tracer.detailedTraceModel.root;
        
        currentOverviewTreeNode = tracer.overviewTraceModel.nodes.get(0);
    }
    
    public void registerStepInformation(int id, String command)
    {
    	System.err.println("Trying to register step information (" + id + "," + command + ")... ");
    	nodeCommands.put(id, command);
        gui.updateAllDisplays();
    }
    
    public void registerRuleApplication(int id, int left, int right, String ruleName)
    {
    	System.err.println("Trying to register rule application (" + id + "," + ruleName + "," + left + "," + right + ")... ");
    	nodeCommands.put(id, "rule(" + ruleName + ")");
    	ChartEdge currentEdge = new ChartEdge(left,right, ruleName, ChartEdge.ACTIVE, true);
    	ChartModelChange cmc = new ChartModelChange(1,currentEdge);
    	addChartChange(id,cmc);
    	activeEdgeStack.add(0,currentEdge);
    	
		TreeModelNode newNode = new TreeModelNode(id, ruleName);
        tracer.overviewTraceModel.addNode(newNode);
        currentOverviewTreeNode.children.add(id);
        newNode.parent = currentOverviewTreeNode.id;
        currentOverviewTreeNode = newNode;
        stepStatus.put(id, Step.STATUS_PROGRESS);
        edgeRegister.put(id, currentEdge);
        lastEdge = currentEdge;
        
        gui.updateAllDisplays();
    }
    
    public void registerStepLocation(String callStack)
    {
    	System.err.println("Trying to register step location (" + callStack + ")... ");
    	List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
    	int stepID = stack.get(0);
    	XMLTraceNode newNode = tracer.registerStepAsChildOf(currentDecisionTreeNode, stepID, stepID, nodeCommands.getData(stepID));
    	traceNodes.put(stepID, newNode);
    	currentDecisionTreeNode = stepID;
    	gui.traceNodeID = stepID;
        if (nodeCommands.getData(stepID).startsWith("rule_close"))
        {
    		TreeModelNode newOverviewNode = new TreeModelNode(stepID, lastEdge.desc);
            tracer.overviewTraceModel.addNode(newOverviewNode);
            currentOverviewTreeNode.children.add(stepID);
            newOverviewNode.parent = currentOverviewTreeNode.id;
            currentOverviewTreeNode = newOverviewNode;
            edgeRegister.put(stepID, lastEdge);
            stepStatus.put(stepID, Step.STATUS_PROGRESS);
            gui.updateAllDisplays();
            gui.selectChartEdge(lastEdge);
        }
        else if (nodeCommands.getData(stepID).startsWith("rule"))
		{
        	gui.updateAllDisplays();
            gui.selectChartEdge(lastEdge);
		}
        else
        {
        	gui.updateAllDisplays();
        }
    }
    
    public void registerStepExit(String callStack)
    {
    	System.err.println("Trying to register step exit (" + callStack + ")... ");
    	List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
    	int stepID = stack.remove(0);
    	gui.nodeColorings.put(stepID, Color.GREEN);
    	gui.traceNodeID = stepID;
        gui.updateAllDisplays();
    }
    
    public void registerStepFinished(String callStack)
    {
    	System.err.println("Trying to register step finished (" + callStack + ")... ");
    	List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
    	int stepID = stack.remove(0);	
    	gui.nodeColorings.put(stepID, Color.BLUE);
    	currentDecisionTreeNode = stack.remove(0);
    	gui.traceNodeID = currentDecisionTreeNode;
    	if (nodeCommands.getData(stepID).startsWith("rule_close"))
    	{
    		stepStatus.put(stepID, Step.STATUS_SUCCESS);
    		//move up one level in overview tree
    		currentOverviewTreeNode = tracer.overviewTraceModel.nodes.get(currentOverviewTreeNode.parent);
    		lastEdge = edgeRegister.getData(currentOverviewTreeNode.id);
    	}
    	gui.selectChartEdge(lastEdge);
    }
    
    public void registerStepFailure(String callStack)
    {  	
    	System.err.println("Trying to register step failure (" + callStack + ")... ");
    	List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
    	int stepID = stack.remove(0);
    	String command = nodeCommands.getData(stepID);
    	//need to handle bug: step failure is called even if edge was successful
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
    		}
    		//current rule application failed; adapt chart accordingly
    		else
    		{
    			System.err.println("Failed edge! Leaving it on the chart as junk...");
    			currentEdge.status = ChartEdge.FAILED;
    			currentEdge.active = false;
                stepStatus.put(stepID, Step.STATUS_FAILURE);
    		}
    		//move up one level in overview tree
    		currentOverviewTreeNode = tracer.overviewTraceModel.nodes.get(currentOverviewTreeNode.parent);
    		lastEdge = edgeRegister.getData(currentOverviewTreeNode.id);
    	}
    	gui.nodeColorings.put(stepID, Color.RED);
    	currentDecisionTreeNode = stack.remove(0);
    	gui.traceNodeID = currentDecisionTreeNode;
    	gui.selectChartEdge(lastEdge);
    }
    
    public void registerChartEdge(int number, int left, int right, String ruleName)
    {
    	System.err.println("Trying to register chartEdge (" + number + "," + left + "," + right + "," + ruleName + ")... ");
    	lastEdge = new ChartEdge(left,right, number + " " + ruleName, ChartEdge.SUCCESSFUL, true);
		ChartModelChange cmc = new ChartModelChange(1,lastEdge);
		int dtNode = findRuleAncestor(currentDecisionTreeNode);
		addChartChange(dtNode,cmc);
		if (activeEdgeStack.size() > 0)
		{
			System.err.println("Marking the following edge as successful: " + activeEdgeStack.get(0));
			successfulEdges.add(activeEdgeStack.get(0));
		}
		currentDecisionTreeHead = traceNodes.getData(dtNode);
		gui.traceNodeID = dtNode;
        gui.updateAllDisplays();
    }
    
    private int findRuleAncestor(int dtNode)
    {
    	XMLTraceNode node = traceNodes.getData(dtNode);
    	while (!nodeCommands.getData(node.id).startsWith("rule(") && !nodeCommands.getData(node.id).equals("init"))
    	{
    		node = node.getParent();
    	}
    	return node.id;
    }
    
    public char getPressedButton()
    {
    	char oldReply = reply;
    	if (reply == 'l') return 'c';
    	reply = 'n';
    	return oldReply;
    }
    
    public void addChartChange(int nodeID, ChartModelChange cmc)
    {
    	if (chartChanges.getData(nodeID) == null)
    	{
    		chartChanges.put(nodeID,new ArrayList<ChartModelChange>());
    	}
    	chartChanges.getData(nodeID).add(cmc);
    }
    
    public static void main(String[] args) throws InterruptedException
    {
    	TraleSld sld = new TraleSld();
    	sld.initializeParseTrace("[it,walks]");
    	Thread.sleep(500);
    	sld.registerChartEdge(0,1,2,"lexicon");	
    	Thread.sleep(500);
    	sld.registerStepInformation(1, "rule_close");
    	sld.registerStepLocation("[1]");
    	Thread.sleep(500);
    	sld.registerRuleApplication(2,1,3,"head_subject");
    	sld.registerStepLocation("[2,1]");
    	Thread.sleep(500);
    	sld.registerStepFailure("[2,1]");
    	Thread.sleep(500);
    	sld.registerStepInformation(3, "unify");
    	sld.registerStepLocation("[3,2,1]");
    	Thread.sleep(500);
    	sld.registerStepInformation(4, "type");
    	sld.registerStepLocation("[4,3,2,1]");
    	Thread.sleep(500);
    	sld.registerRuleApplication(5,0,2,"head_complement");
    	sld.registerStepLocation("[5,4,3,2,1]");
    	Thread.sleep(500);
    	sld.registerChartEdge(5,0,1,"lexicon");	
    	Thread.sleep(500);
    	sld.registerStepExit("[5,1]");
    }
}
