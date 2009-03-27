package tralesld.visual.chart;

import tralesld.struct.chart.*;

public class ChartViewBuilder
{
    public static ChartView buildChartView(ChartModel cm, boolean junkEdges)
    {
        ChartView v = new ChartView();
        v.words = cm.words;
        for (ChartEdge e : cm.edges)
        {
            //do not display junk edges, i.e. edges that have failed
            if (e.status == ChartEdge.FAILED && !junkEdges) continue;
            int curSize = v.usedSpace.size();
            for (int i = 0; i <= curSize; i++)
            {    
                if (i == curSize)
                {
                    v.usedSpace.add(new Boolean[cm.size]);
                    for (int j = 0; j < cm.size; j++)
                    {
                        if (j >= e.l && j < e.r)
                        {
                            v.usedSpace.get(i)[j] = true;
                        }
                        else
                        {
                            v.usedSpace.get(i)[j] = false;
                        }
                    }
                    ChartViewEdge ve = new ChartViewEdge(e,e.l * v.cellWidth, i * v.cellHeight, v.cellWidth * (e.r - e.l), v.cellHeight);
                    v.edges.add(ve);
                }
                else
                {
                    boolean match = true;
                    for (int j = e.l; j < e.r; j++)
                    {
                        if (v.usedSpace.get(i)[j] == true)
                        {
                            match = false;
                        }
                    }
                    if (match)
                    {   
                        for (int j = e.l; j < e.r; j++)
                        {
                            v.usedSpace.get(i)[j] = true;
                        }
                        ChartViewEdge ve = new ChartViewEdge(e,e.l * v.cellWidth, i * v.cellHeight, v.cellWidth * (e.r - e.l), v.cellHeight);
                        v.edges.add(ve);
                        break;
                    }
                }
            }
        }
        return v;
    }
}
