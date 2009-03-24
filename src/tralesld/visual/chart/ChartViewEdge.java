package tralesld.visual.chart;

import tralesld.struct.chart.*;

public class ChartViewEdge extends ChartEdge
{
    //additional information: where and how to display edge on view pane
    int x;
    int y;
    int height;
    int width;
    
    public ChartViewEdge(ChartEdge edge, int x, int y, int width, int height)
    {
        super(edge.id, edge.l, edge.r, edge.desc, edge.status, edge.active);
        this.x = x;
        this.y = y;
        this.height = height;
        this.width = width;
    }
}
