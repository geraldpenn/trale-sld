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
    public DataStore<ChartModelChange> chartChanges;
    public DataStore<XMLTraceNode> traceNodes;
    public DataStore<String> nodeCommands;
    
    public Tracer tracer;
    public XMLTraceModel traceModel;
    
    int currentDecisionTreeNode = 0;
    
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
    	chartChanges = new DataStore<ChartModelChange>();
    	traceNodes = new DataStore<XMLTraceNode>();
    	nodeCommands = new DataStore<String>();
    	nodeCommands.put(0, "init");
    	
    	tracer = new Tracer();
    	traceModel = tracer.traceModel;
    	traceNodes.put(0, traceModel.root);
    }
    
    public void registerStepInformation(int id, String command)
    {
    	System.err.println("Trying to register step information (" + id + "," + command + ")... ");
    	nodeCommands.put(id, command);
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
    	gui.updateTreePanelDisplay();
    }
    
    public void registerChartEdge(int number, int left, int right, String ruleName)
    {
    	System.err.println("Trying to register chartEdge (" + number + "," + left + "," + right + "," + ruleName + ")... ");
    	ChartModelChange cmc = new ChartModelChange(1,new ChartEdge(left,right, number + " " + ruleName, ChartEdge.SUCCESSFUL, true));
    	int dtNode = findRuleAncestor(currentDecisionTreeNode);
    	chartChanges.put(dtNode,cmc);
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
    
    public static void main(String[] args)
    {
    	TraleSld sld = new TraleSld();
    	sld.initializeParseTrace("[it,walks]");
    	sld.registerChartEdge(0,1,2,"lexicon");	
    	sld.registerStepInformation(1, "rule_close");
    	sld.registerStepLocation("[1]");
    	sld.registerStepInformation(2, "rule");
    	sld.registerStepLocation("[2,1]");
    	sld.registerStepInformation(3, "unify");
    	sld.registerStepLocation("[3,2,1]");
    	sld.registerStepInformation(4, "type");
    	sld.registerStepLocation("[4,3,2,1]");
    	sld.registerChartEdge(2,0,2,"head_complement");	
    }
}
