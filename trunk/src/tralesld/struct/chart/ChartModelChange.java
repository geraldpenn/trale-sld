package tralesld.struct.chart;


public class ChartModelChange
{
    int type;
    public ChartEdge edge;
    
    static final int REMOVE_EDGE = -1;
    static final int ADD_EDGE = 1;
    
    public ChartModelChange(int type, ChartEdge edge)
    {
        this.type = type;
        this.edge = edge;
    }
}
