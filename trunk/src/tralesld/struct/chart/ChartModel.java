package tralesld.struct.chart;

import java.util.*;


public class ChartModel
{
    public int size;
    public int maxSize;
    public List<String> words;
    public Map<Integer, ChartEdge> edges;
    
    public ChartModel(List<String> words)
    {
        this.size = words.size();
        this.words = words;
        edges = new HashMap<Integer,ChartEdge>();
        maxSize = this.size * 2;
    }
    
    public void processChange(ChartModelChange change)
    {
        if (change.type == ChartModelChange.ADD_EDGE)
        {
            edges.put(change.edge.id, change.edge);
        }
        else
        {
            edges.remove(change.edge.id);
        }
    }
}
