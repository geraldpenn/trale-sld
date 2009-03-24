package tralesld.visual.chart;

import java.awt.*;
import javax.swing.*;

import tralesld.struct.chart.*;

public class ChartViewPanel extends JPanel
{
    ChartView v;
    
    public ChartViewPanel()
    {
        v = new ChartView();
    }
    
    public void paintComponent(Graphics canvas)
    {
        Graphics2D cnv = (Graphics2D) canvas;
        //cnv.translate(this.getX(), this.getY());
        cnv.setColor(Color.WHITE);
        cnv.fillRect(0, 0, v.usedSpace.get(0).length * v.cellWidth, v.usedSpace.size() * v.cellHeight);
        for (ChartViewEdge cve : v.edges)
        {
            if (cve.status == ChartEdge.ACTIVE)
            {
                cnv.setColor(Color.CYAN);
            }
            else if (cve.status == ChartEdge.SUCCESSFUL)
            {
                cnv.setColor(Color.GREEN);
            }
            else if (cve.status == ChartEdge.FAILED)
            {
                cnv.setColor(Color.RED);
            }
            cnv.fillRect(cve.x, cve.y, cve.width, cve.height);
            if (cve.active)
            {
                cnv.setStroke(new BasicStroke(2));
                cnv.setFont(new Font(cnv.getFont().getFontName(),Font.BOLD, 12));
            }
            else
            {
                cnv.setStroke(new BasicStroke(1));
                cnv.setFont(new Font(cnv.getFont().getFontName(),Font.PLAIN, 12));
            }
            cnv.setColor(Color.BLACK);
            cnv.drawRect(cve.x, cve.y, cve.width, cve.height);
            cnv.drawString(cve.desc, cve.x + 2, cve.y + 14);
        }
    }
}
