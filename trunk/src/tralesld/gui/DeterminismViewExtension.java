package tralesld.gui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.util.Set;

import tralesld.visual.tree.TreeView;
import tralesld.visual.tree.TreeViewExtension;
import tralesld.visual.tree.TreeViewNode;
import tralesld.visual.tree.TreeViewPanel;

public class DeterminismViewExtension extends TreeViewExtension
{
    Set<Integer> deterministicallyExited;
    
    public DeterminismViewExtension(Set<Integer> deterministicallyExited)
    {
        this.deterministicallyExited = deterministicallyExited;
    }
    
    public void paintOnTreePanel(TreeViewPanel panel, Graphics2D canvas)
    {
        TreeView view = panel.t;
        canvas.setStroke(new BasicStroke(1));
        canvas.setColor(Color.BLACK);
        canvas.setFont(new Font(canvas.getFont().getFamily(),Font.PLAIN, 12));
        for (int childID = 0, sucChildID = 0; sucChildID < view.treeNodes.size(); childID++)
        {   
            TreeViewNode n = view.treeNodes.get(childID);
            if (n == null) continue;
            sucChildID++;

            //draw little box to the right of the node displaying determinism information 
            FontMetrics fm = canvas.getFontMetrics();
            int width = fm.stringWidth(n.tag);
            int x = n.x + width/2 + 6;
            if (panel.getNodePositioning() == TreeViewPanel.LEFT_ALIGNMENT)
            {
                x = n.x + width + 6;
            }
            x += panel.t.getIndent(childID);
            int y = n.y - 10;
            canvas.drawRect(x - 4, y, 4, 12);
            canvas.drawRect(x, y, 12, 12);
            if (deterministicallyExited.contains(childID))
            {
                canvas.drawString("X", x + 6, n.y);
            }
            else
            {
                canvas.drawString("?", x + 6, n.y);
            }
        }
    }
}
