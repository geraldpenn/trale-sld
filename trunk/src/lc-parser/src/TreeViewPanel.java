import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import java.util.HashMap;
import java.awt.*;

import javax.swing.JPanel;

public class TreeViewPanel extends JPanel implements MouseListener
{
    // serialVersionUID to avoid warning
    private static final long serialVersionUID = 1L;

    public TreeView t;

    public boolean edgyLines;

    private HashMap<String, Integer> eventGrid;

    public TreeViewPanel()
    {
        t = null;
        eventGrid = new HashMap<String, Integer>();
        edgyLines = false;
        this.addMouseListener(this);
    }

    public void paint(Graphics canvas)
    {
        if (t != null)
        {
            // clear canvas
            Dimension newD = new Dimension(t.totalTreeWidth, t.totalTreeHeight);
            this.setSize(newD);
            this.setMinimumSize(newD);
            this.setMaximumSize(newD);
            this.setPreferredSize(newD);
            canvas.setColor(Color.WHITE);
            canvas.fillRect(0, 0, 2000, 2000);
            canvas.fillRect(0, 0, this.getSize().width, this.getSize().height);
            // print information on current tree
            for (int i = 0; i < t.nodeLevels.size(); i++)
            {
                ArrayList<Integer> nodes = t.nodeLevels.get(i);
                for (int j = 0; j < nodes.size(); j++)
                {
                    // print box around node tag
                    int x = t.treeNodes.get(nodes.get(j)).x - t.treeNodes.get(nodes.get(j)).tag.length() * 4;
                    int y = t.treeNodes.get(nodes.get(j)).y - 10;
                    int width = t.treeNodes.get(nodes.get(j)).tag.length() * 8;
                    canvas.setColor(Color.WHITE);
                    canvas.fillRect(x, y, width, 12);
                    canvas.setColor(Color.BLACK);
                    canvas.drawRect(x, y, width, 12);
                    // markObjectArea(nodes.get(j),x,y,width,12);

                    // print tag name of node
                    x = t.treeNodes.get(nodes.get(j)).x - t.treeNodes.get(nodes.get(j)).tag.length() * 3;
                    y = t.treeNodes.get(nodes.get(j)).y;
                    String tag = t.treeNodes.get(nodes.get(j)).tag;
                    // newText.setAttribute("font-size","10");
                    canvas.drawString(tag, x, y);
                }
            }
            // create lines and their tags
            canvas.setColor(Color.BLACK);
            for (int i = 1; i < t.nodeLevels.size(); i++)
            {
                ArrayList<Integer> nodes = t.nodeLevels.get(i);
                for (int j = 0; j < nodes.size(); j++)
                {
                    int x1 = t.treeNodes.get(t.treeNodes.get(nodes.get(j)).parent).x;
                    int y1 = t.treeNodes.get(nodes.get(j)).y - t.treeLevelHeight + 2;
                    int x2 = t.treeNodes.get(nodes.get(j)).x;
                    int y2 = t.treeNodes.get(nodes.get(j)).y - 10;
                    if (edgyLines)
                    {
                        drawLineAccordingToType(canvas,t.treeNodes.get(nodes.get(j)).edgeType,x1, y1, x2, y1);
                        drawLineAccordingToType(canvas,t.treeNodes.get(nodes.get(j)).edgeType,x2, y1, x2, y2);
                    }
                    else
                    {
                        drawLineAccordingToType(canvas,t.treeNodes.get(nodes.get(j)).edgeType,x1, y1, x2, y2);
                    }
                    canvas.drawString(t.treeNodes.get(nodes.get(j)).edgeTag, (x1 + x2) / 2, (y1 + y2) / 2);
                    String edgeDir = t.treeNodes.get(nodes.get(j)).edgeDir;
                    if (edgeDir.equals("up"))
                    {
                        double slope = ((y1 - y2 + 0.1) / (x1 - x2 + 0.1));
                        double lowerSlope = (slope - 1) / (1 + slope);
                        double higherSlope = -(1 / lowerSlope);
                        Polygon arrowhead = new Polygon();
                        arrowhead.addPoint(x1, y1);
                        if ((x2 <= x1 && slope < -1) || (x2 >= x1 && slope > -1))
                            arrowhead.addPoint(x1 + (int) (10 / Math.sqrt(1 + lowerSlope * lowerSlope)), y1 + (int) ((10 / Math.sqrt(1 + lowerSlope * lowerSlope)) * lowerSlope));
                        if ((x2 <= x1 && slope > -1) || (x2 > x1 && slope < -1))
                            arrowhead.addPoint(x1 - (int) (10 / Math.sqrt(1 + lowerSlope * lowerSlope)), y1 - (int) ((10 / Math.sqrt(1 + lowerSlope * lowerSlope)) * lowerSlope));
                        if ((x2 <= x1 && slope > 1) || (x2 > x1 && slope < 1))
                            arrowhead.addPoint(x1 + (int) (10 / Math.sqrt(1 + higherSlope * higherSlope)), y1 + (int) ((10 / Math.sqrt(1 + higherSlope * higherSlope)) * higherSlope));
                        if ((x2 <= x1 && slope < 1) || (x2 >= x1 && slope > 1))
                            arrowhead.addPoint(x1 - (int) (10 / Math.sqrt(1 + higherSlope * higherSlope)), y1 - (int) ((10 / Math.sqrt(1 + higherSlope * higherSlope)) * higherSlope));
                        canvas.fillPolygon(arrowhead);
                    }
                    else if (edgeDir.equals("down"))
                    {
                        double slope = ((y2 - y1 - 0.1) / (x2 - x1 + 0.1));
                        double lowerSlope = (slope - 1) / (1 + slope);
                        double higherSlope = -(1 / lowerSlope);
                        Polygon arrowhead = new Polygon();
                        arrowhead.addPoint(x2, y2);
                        if ((x2 < x1 && slope < -1) || (x2 >= x1 && slope > -1))
                            arrowhead.addPoint(x2 - (int) (10 / Math.sqrt(1 + lowerSlope * lowerSlope)), y2 - (int) ((10 / Math.sqrt(1 + lowerSlope * lowerSlope)) * lowerSlope));
                        if ((x2 < x1 && slope > -1) || (x2 > x1 && slope < -1))
                            arrowhead.addPoint(x2 + (int) (10 / Math.sqrt(1 + lowerSlope * lowerSlope)), y2 + (int) ((10 / Math.sqrt(1 + lowerSlope * lowerSlope)) * lowerSlope));
                        if ((x2 < x1 && slope > 1) || (x2 > x1 && slope < 1))
                            arrowhead.addPoint(x2 - (int) (10 / Math.sqrt(1 + higherSlope * higherSlope)), y2 - (int) ((10 / Math.sqrt(1 + higherSlope * higherSlope)) * higherSlope));
                        if ((x2 < x1 && slope < 1) || (x2 >= x1 && slope > 1))
                            arrowhead.addPoint(x2 + (int) (10 / Math.sqrt(1 + higherSlope * higherSlope)), y2 + (int) ((10 / Math.sqrt(1 + higherSlope * higherSlope)) * higherSlope));
                        canvas.fillPolygon(arrowhead);
                    }
                }
            }
        }
    }

    public void mousePressed(MouseEvent e)
    {
    }

    public void mouseReleased(MouseEvent e)
    {
    }

    public void mouseEntered(MouseEvent e)
    {
    }

    public void mouseExited(MouseEvent e)
    {
    }

    public void mouseClicked(MouseEvent e)
    {
        String event = "c" + e.getX() + "/" + e.getY();
        if (eventGrid.get(event) != null)
        {
            repaint();
        }
        else
        {
            if (e.getButton() != MouseEvent.BUTTON1)
            {
                // popup: to be reactivated
                // display.panePopup.show(this,e.getX(),e.getY());
            }
        }
    }

    /*
     * private void markObjectArea(int id, int x, int y, int width, int height) {
     * for (int i = x; i < x + width; i++) { for (int j = y; j < y + height;
     * j++) { eventGrid.put("c" + i + "/" + j, id); } } }
     */

    public void toggleEdgyLines()
    {
        if (edgyLines)
        {
            edgyLines = false;
        }
        else
        {
            edgyLines = true;
        }
    }

    public void displayTreeView(TreeView t)
    {
        this.t = t;
        repaint();
    }
    
    public static void drawLineAccordingToType(Graphics g, String type, int x0, int y0, int x1, int y1)
    {
        if (type.equals("dotted"))
        {
            drawDottedLine(g,x0,y0,x1,y1,g.getColor(),1,1);
        }
        else
        {
            g.drawLine(x0,y0,x1,y1);
        }
    }

    public static void drawDottedLine(Graphics g, int x0, int y0, int x1, int y1, Color color, int dashLen, int spaceLen)
    {
        Color c = g.getColor();
        g.setColor(color);
        int dx = x1 - x0;
        int dy = y1 - y0;
        float t = 0.5f;

        g.setColor(color);
        g.drawLine(x0, y0, x0, y0);

        int dashCount = 0;
        int spaceCount = 0;
        boolean doPlot = dashLen > 1;

        if (Math.abs(dx) > Math.abs(dy))
        { // slope < 1
            float m = (float) dy / (float) dx; // compute slope
            t += y0;
            dx = (dx < 0) ? -1 : 1;
            m *= dx;
            while (x0 != x1)
            {
                x0 += dx; // step to next x value
                t += m;
                if (doPlot)
                {
                    g.drawLine(x0, (int) t, x0, (int) t);
                    dashCount++;
                    if (dashCount >= dashLen)
                    {
                        dashCount = 0;
                        spaceCount = 0;
                        doPlot = false;
                    }
                }
                else
                {
                    spaceCount++;
                    if (spaceCount >= spaceLen)
                    {
                        spaceCount = 0;
                        dashCount = 0;
                        doPlot = true;
                    }
                }

            }
        }
        else if (dy != 0)
        { // slope >= 1
            float m = (float) dx / (float) dy; // compute slope
            t += x0;
            dy = (dy < 0) ? -1 : 1;
            m *= dy;
            while (y0 != y1)
            {
                y0 += dy; // step to next y value
                t += m;
                if (doPlot)
                {
                    g.drawLine((int) t, y0, (int) t, y0);
                    dashCount++;
                    if (dashCount >= dashLen)
                    {
                        dashCount = 0;
                        spaceCount = 0;
                        doPlot = false;
                    }
                }
                else
                {
                    spaceCount++;
                    if (spaceCount >= spaceLen)
                    {
                        spaceCount = 0;
                        dashCount = 0;
                        doPlot = true;
                    }
                }
            }
        }
        g.setColor(c);
    }
}
