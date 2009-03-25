package tralesld.struct.chart;

import java.util.*;


public class ChartModel
{
    public int size;
    public List<String> words;
    public List<ChartEdge> edges;
    
    public ChartModel(List<String> words)
    {
        this.size = words.size();
        this.words = words;
        edges = new LinkedList<ChartEdge>();
    }
    
    public void processChange(ChartModelChange change)
    {
        if (change.type == ChartModelChange.ADD_EDGE)
        {
            edges.add(change.edge);
        }
        else
        {
            edges.remove(change.edge);
        }
    }
}
