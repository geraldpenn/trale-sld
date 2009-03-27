package tralesld;

import java.awt.Color;
import java.util.*;

import tralesld.gui.*;
import tralesld.storage.*;
import tralesld.struct.chart.*;
import tralesld.struct.trace.*;
import tralesld.util.*;
import tralesld.visual.tree.TreeView;

public class TraleSld
{
    TraleSldGui gui;
    
    //current chart model
    public ChartModel curCM;
    
    //chart is stored in form of chart changes for each trace node
    public DataStore<List<ChartModelChange>> chartChanges;
    public DataStore<XMLTraceNode> traceNodes;
    public DataStore<String> nodeCommands;
    
    public Tracer tracer;
    public XMLTraceModel traceModel;
    
    int currentDecisionTreeNode = 0;
    LinkedList<ChartEdge> activeEdgeStack;
    HashSet<ChartEdge> successfulEdges;
    
    //current signal to prolog
    public char reply = 'n';
    
    public TraleSld()
    {
    	System.err.print("Trying to build GUI window... ");
        gui = TraleSldGui.createAndShowGUI(this);
        System.err.println("Success.");
    }
    
    public void initializeParseTrace(String parsedSentenceList)
    {
    	System.err.println("Trying to initialize parse trace... ");
    	List<String> wordList = PrologUtilities.parsePrologStringList(parsedSentenceList);
    	curCM = new ChartModel(wordList);
    	chartChanges = new DataStore<List<ChartModelChange>>();
    	traceNodes = new DataStore<XMLTraceNode>();
    	nodeCommands = new DataStore<String>();
    	nodeCommands.put(0, "init");
    	
    	activeEdgeStack = new LinkedList<ChartEdge>();
    	successfulEdges = new HashSet<ChartEdge>();
    	
    	tracer = new Tracer();
    	traceModel = tracer.traceModel;
    	traceNodes.put(0, traceModel.root);
    }
    
    public void registerStepInformation(int id, String command)
    {
    	System.err.println("Trying to register step information (" + id + "," + command + ")... ");
    	nodeCommands.put(id, command);
    }
    
    public void registerRuleApplication(int id, int left, int right, String ruleName)
    {
    	System.err.println("Trying to register rule application (" + id + "," + ruleName + "," + left + "," + "right" + ")... ");
    	nodeCommands.put(id, "rule(" + ruleName + ")");
    	ChartEdge currentEdge = new ChartEdge(left,right, ruleName, ChartEdge.ACTIVE, true);
    	ChartModelChange cmc = new ChartModelChange(1,currentEdge);
    	addChartChange(id,cmc);
    	activeEdgeStack.add(0,currentEdge);
    	gui.updateChartPanelDisplay();
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
    	gui.updateTreePanelDisplay();
    	gui.updateChartPanelDisplay();
    }
    
    public void registerStepExit(String callStack)
    {
    	System.err.println("Trying to register step exit (" + callStack + ")... ");
    	List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
    	int stepID = stack.remove(0);
    	gui.nodeColorings.put(stepID, Color.GREEN);
    	gui.updateChartPanelDisplay();
    }
    
    public void registerStepFinished(String callStack)
    {
    	System.err.println("Trying to register step finished (" + callStack + ")... ");
    	List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
    	int stepID = stack.remove(0);	
    	gui.nodeColorings.put(stepID, Color.BLUE);
    	currentDecisionTreeNode = stack.remove(0);
    	gui.updateChartPanelDisplay();
    }
    
    public void registerStepFailure(String callStack)
    {  	
    	System.err.println("Trying to register step failure (" + callStack + ")... ");
    	List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
    	int stepID = stack.remove(0);
    	String command = nodeCommands.getData(stepID);
    	//need to handle bug: step failure is called even if edge was successful
    	if (command.startsWith("rule"))
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
    		}
    		//current rule application failed; adapt chart accordingly
    		else
    		{
    			System.err.println("Failed edge! Leaving it on the chart as junk...");
    			currentEdge.status = ChartEdge.FAILED;
    			currentEdge.active = false;
    		}
    	}
    	gui.nodeColorings.put(stepID, Color.RED);
    	currentDecisionTreeNode = stack.remove(0);
    	gui.updateChartPanelDisplay();
    }
    
    public void registerChartEdge(int number, int left, int right, String ruleName)
    {
    	System.err.println("Trying to register chartEdge (" + number + "," + left + "," + right + "," + ruleName + ")... ");
		ChartModelChange cmc = new ChartModelChange(1,new ChartEdge(left,right, number + " " + ruleName, ChartEdge.SUCCESSFUL, true));
		int dtNode = findRuleAncestor(currentDecisionTreeNode);
		addChartChange(dtNode,cmc);
		if (activeEdgeStack.size() > 0)
		{
			System.err.println("Marking the following adge as succesful: " + activeEdgeStack.get(0));
			successfulEdges.add(activeEdgeStack.get(0));
		}
    	gui.updateChartPanelDisplay();
    }
    
    private int findRuleAncestor(int dtNode)
    {
    	XMLTraceNode node = traceNodes.getData(dtNode);
    	while (!nodeCommands.getData(node.id).equals("rule") && !nodeCommands.getData(node.id).equals("init"))
    	{
    		node = node.getParent();
    	}
    	return node.id;
    }
    
    public char getPressedButton()
    {
    	char oldReply = reply;
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
    	sld.registerRuleApplication(2,1,2,"head_subject");
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
    	sld.registerChartEdge(5,0,2,"head_complement");	
    	Thread.sleep(500);
    	sld.registerStepExit("[5,1]");
    }
}
