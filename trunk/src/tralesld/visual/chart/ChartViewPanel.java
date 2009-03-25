package tralesld.visual.chart;

import java.awt.*;
import javax.swing.*;

import tralesld.struct.chart.*;

public class ChartViewPanel extends JPanel
{
    public ChartView v;
    
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
        int maxY = (v.usedSpace.size() - 1) * v.cellHeight;
        for (ChartViewEdge cve : v.edges)
        {
            if (cve.status == ChartEdge.ACTIVE)
            {
                cnv.setColor(Color.CYAN);
            }
            else if (cve.status == ChartEdge.SUCCESSFUL)
            {
                if (cve.active)
                {
                    cnv.setColor(Color.GREEN);
                }
                else
                {
                    cnv.setColor(new Color(150,255,150));
                }
            }
            else if (cve.status == ChartEdge.FAILED)
            {
                if (cve.active)
                {
                    cnv.setColor(Color.RED);
                }
                else
                {
                    cnv.setColor(new Color(255,150,150));
                }
            }
            cnv.fillRect(cve.x, maxY - cve.y, cve.width, cve.height);
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
            cnv.drawRect(cve.x, maxY - cve.y, cve.width, cve.height);
            cnv.drawString(cve.desc, cve.x + 2, maxY - cve.y + 14);
        }
    }
}
