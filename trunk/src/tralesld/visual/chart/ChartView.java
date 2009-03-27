package tralesld.visual.chart;

import java.util.*;
import tralesld.struct.chart.*;

public class ChartView
{
    //display options
    int cellWidth = 150;
    int cellHeight = 20;    
    
    ArrayList<ChartViewEdge> edges;
    ArrayList<Boolean[]> usedSpace;
    
    List<String> words;
    
    public ChartView()
    {
        edges = new ArrayList<ChartViewEdge>();
        usedSpace = new ArrayList<Boolean[]>();
        words = new ArrayList<String>();
    }
}
