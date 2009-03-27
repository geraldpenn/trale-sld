package tralesld;

import java.util.*;

import tralesld.gui.*;
import tralesld.storage.*;
import tralesld.struct.chart.*;
import tralesld.struct.trace.*;
import tralesld.util.*;

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
    ChartEdge currentEdge;
    
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
    	
    	currentEdge = null;
    	
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
    	nodeCommands.put(id, "rule");
    	currentEdge = new ChartEdge(left,right, ruleName, ChartEdge.ACTIVE, true);
    	ChartModelChange cmc = new ChartModelChange(1,currentEdge);
    	addChartChange(id,cmc);
    	gui.updateChartPanelDisplay();
    }
    
    public void registerStepLocation(String callStack)
    {
    	System.err.println("Trying to register step location (" + callStack + ")... ");
    	List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
    	int stepID = stack.get(0);
    	XMLTraceNode newNode = tracer.registerStep(stack, stepID, nodeCommands.getData(stepID));
    	traceNodes.put(stepID, newNode);
    	currentDecisionTreeNode = stepID;
    	System.err.println("Current decision tree node: " + stepID);
    	System.err.println("Current number of nodes in decision tree: " + traceModel);
    	gui.traceNodeID = stepID;
    	gui.updateTreePanelDisplay();
    	gui.updateChartPanelDisplay();
    }
    
    public void registerStepFailure(int id)
    {
    	String command = nodeCommands.getData(id);
    	if (command.equals("rule"))
    	{
    		//current rule application failed; adapt chart accordingly
    		currentEdge.status = ChartEdge.FAILED;
    		currentEdge.active = false;
    		currentEdge = null;
    	}
    }
    
    public void registerChartEdge(int number, int left, int right, String ruleName)
    {
    	System.err.println("Trying to register chartEdge (" + number + "," + left + "," + right + "," + ruleName + ")... ");
    	if (ruleName.equals("lexicon"))
    	{
    		ChartModelChange cmc = new ChartModelChange(1,new ChartEdge(left,right, number + " " + ruleName, ChartEdge.SUCCESSFUL, true));
    		int dtNode = findRuleAncestor(currentDecisionTreeNode);
    		addChartChange(dtNode,cmc);
    	}
    	else
    	{
    		currentEdge.status = ChartEdge.SUCCESSFUL;
    		currentEdge.desc = number + " " + ruleName;
    		currentEdge = null;
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
    	sld.registerStepFailure(2);
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
    }
}
