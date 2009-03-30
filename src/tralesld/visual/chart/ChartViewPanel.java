package tralesld.visual.chart;

import java.awt.*;
import java.util.HashMap;

import javax.swing.*;

import tralesld.struct.chart.*;

public class ChartViewPanel extends JPanel
{
    public ChartView v;
    public boolean displayFailedEdges;
	
	public HashMap<String, Integer> eventGrid;
    
    public ChartViewPanel()
    {
        v = new ChartView();
        displayFailedEdges = true;
        eventGrid = new HashMap<String, Integer>();
    }
    
    public void paintComponent(Graphics canvas)
    {
    	if (v == null) return;
        Graphics2D cnv = (Graphics2D) canvas;
        //cnv.translate(this.getX(), this.getY());
        cnv.setColor(Color.WHITE);
        cnv.fillRect(0, 0, v.words.size() * 2 * v.cellWidth + 5, 30 * v.cellHeight);
        int maxY = (v.usedSpace.size() - 1) * v.cellHeight;
        //cosmetic improvement of frame
        cnv.setColor(Color.BLACK);
        cnv.setStroke(new BasicStroke(2));
        cnv.drawLine(5, 5, 5, maxY + 25);
        cnv.drawLine(v.words.size() * v.cellWidth + 5, 5, v.words.size() * v.cellWidth + 5, maxY + 25);
        cnv.drawLine(5, 5, v.words.size() * v.cellWidth + 5, 5);
        cnv.drawLine(5, maxY + 25, v.words.size() * v.cellWidth + 5, maxY + 25);
        //draw all the edges
        for (ChartViewEdge cve : v.edges)
        {
            if (cve.status == ChartEdge.ACTIVE)
            {
            	if (cve.active)
            	{
            		cnv.setColor(new Color(0,200,200));
            	}
            	else
            	{
            		cnv.setColor(new Color(127,255,255));
            	}
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
            cnv.fillRect(cve.x + 5, maxY - cve.y + 5, cve.width, cve.height);
            //fill event grid (allow clicking on chart edges)
            for (int i = cve.x + 5; i < cve.x + 5 + cve.width; i++)
            {
                for (int j = maxY - cve.y + 5; j < maxY - cve.y + 5 + cve.height; j++)
                {
                	eventGrid.put(i + "." + j, cve.id);
                }
            }
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
            cnv.drawRect(cve.x + 5, maxY - cve.y + 5, cve.width, cve.height);
            cnv.drawString(cve.desc, cve.x + 7, maxY - cve.y + 19);
        }
        //draw captions
        cnv.setStroke(new BasicStroke(1));
        cnv.setFont(new Font(Font.MONOSPACED,Font.PLAIN, 12));
        cnv.setColor(Color.BLACK);
        for (int i = 0; i < v.words.size(); i++)
        {
        	cnv.drawString(i + " " + v.words.get(i), i * v.cellWidth + 3, maxY + 39);
        }
    }
}
