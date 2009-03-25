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
    
    int currentDecisionTreeNode;
    
    
    public TraleSld()
    {
    	System.err.print("Trying to build GUI window... ");
        gui = TraleSldGui.createAndShowGUI(this);
        System.err.println("Success.");
    }
    
    public void initializeParseTrace(String parsedSentenceList)
    {
    	System.err.print("Trying to initialize parse trace... ");
    	List<String> wordList = PrologUtilities.parsePrologStringList(parsedSentenceList);
    	curCM = new ChartModel(wordList);
    	chartChanges = new DataStore<ChartModelChange>();
    	traceNodes = new DataStore<XMLTraceNode>();
    	nodeCommands = new DataStore<String>();
    	
    	tracer = new Tracer();
    	traceModel = new XMLTraceModel();
    	System.err.println("Success.");
    }
    
    public void registerStepInformation(int id, String command)
    {
    	System.err.print("Trying to register step information (" + id + "," + command + ")... ");
    	nodeCommands.put(id, command);
    	System.err.println("Success.");
    }
    
    public void registerStepLocation(String callStack)
    {
    	System.err.print("Trying to register step location (" + callStack + ")... ");
    	List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
    	int stepID = stack.get(0);
    	XMLTraceNode newNode = tracer.registerStep(stack, stepID, nodeCommands.getData(stepID));
    	traceNodes.put(stepID, newNode);
    	gui.updateTreePanelDisplay();
    	System.err.println("Success.");
    }
    
    public void registerChartEdge(int number, int left, int right, String ruleName)
    {
    	System.err.print("Trying to register chartEdge (" + number + "," + left + "," + right + "," + ruleName + ")... ");
    	ChartModelChange cmc = new ChartModelChange(1,new ChartEdge(left,right, number + " " + ruleName, ChartEdge.SUCCESSFUL, true));
    	int dtNode = findRuleAncestor(currentDecisionTreeNode);
    	chartChanges.put(dtNode,cmc);
    	gui.updateChartPanelDisplay();
    	System.err.println("Success.");
    }
    
    private int findRuleAncestor(int dtNode)
    {
    	XMLTraceNode node = traceNodes.getData(dtNode);
    	while (!nodeCommands.getData(node.id).equals("rule"))
    	{
    		node = node.getParent();
    	}
    	return node.id;
    }
}
