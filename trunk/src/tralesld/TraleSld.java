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
    
    Tracer tracer;
    XMLTraceModel traceModel;
    
    //current node in decision tree
    int dtNode;
    
    public TraleSld()
    {
        gui = TraleSldGui.createAndShowGUI(this);  
    }
    
    public void initializeParseTrace(String parsedSentenceList)
    {
    	List<String> wordList = PrologUtilities.parsePrologStringList(parsedSentenceList);
    	curCM = new ChartModel(wordList);
    	chartChanges = new DataStore<ChartModelChange>();
    	traceNodes = new DataStore<XMLTraceNode>();
    }
    
    public void registerStepInvocation(String callStack)
    {
    	List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
    }
    
    public void registerChartEdge(int number, int left, int right, String ruleName)
    {
    	ChartModelChange cmc = new ChartModelChange(1,new ChartEdge(left,right, number + " " + ruleName, ChartEdge.SUCCESSFUL, true));
    	chartChanges.put(dtNode,cmc);
    }
}
