package tralesld.visual.chart;

import javax.swing.*;
import tralesld.struct.chart.*;

public class ChartViewDemo extends JFrame
{
    ChartViewPanel chartViewPanel;
    
    public ChartViewDemo(ChartModel cm)
    {
        chartViewPanel = new ChartViewPanel();
        chartViewPanel.v = ChartViewBuilder.buildChartView(cm);
        
        chartViewPanel.setBounds(0, 30, 400, 500);
        
        this.setLayout(null);
        this.add(chartViewPanel);
        this.setBounds(0,0,500,600);
        this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
    }
    
    public static void main(String[] args)
    {
        ChartModel cm = new ChartModel(2);
        cm.edges.add(new ChartEdge(0,1,2,"0 lexicon", 1, true));
        cm.edges.add(new ChartEdge(0,0,1,"1 lexicon", 1, false));
        cm.edges.add(new ChartEdge(0,1,2,"2 head_complement", 0, false));
        cm.edges.add(new ChartEdge(0,1,2,"3 head_subject", 0, false));
        cm.edges.add(new ChartEdge(0,0,2,"4 head_complement", 0, false));
        
        ChartViewDemo window = new ChartViewDemo(cm);
        window.setVisible(true);
    }
}
