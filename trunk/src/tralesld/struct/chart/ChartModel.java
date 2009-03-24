package tralesld.struct.chart;

import java.util.*;


public class ChartModel
{
    public int size;
    public List<ChartEdge> edges;
    
    public ChartModel(int size)
    {
        this.size = size;
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
