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
    Set<Integer> nonDetermBecauseOfRedo;

    public DeterminismViewExtension(Set<Integer> deterministicallyExited, Set<Integer> nonDetermBecauseOfRedo)
    {
        this.deterministicallyExited = deterministicallyExited;
        this.nonDetermBecauseOfRedo = nonDetermBecauseOfRedo;
    }

    public void paintOnTreePanel(TreeViewPanel panel, Graphics2D canvas)
    {
        TreeView view = panel.t;
        canvas.setStroke(new BasicStroke(1));
        canvas.setColor(Color.BLACK);
        for (int childID = 0, sucChildID = 0; sucChildID < view.treeNodes.size(); childID++)
        {
            TreeViewNode n = view.treeNodes.get(childID);
            if (n == null)
                continue;
            sucChildID++;

            if (panel.t.getMarkedNodes().contains(childID))
            {
                canvas.setFont(new Font(canvas.getFont().getFamily(), Font.BOLD, 12));
            }
            else
            {
                canvas.setFont(new Font(canvas.getFont().getFamily(), Font.PLAIN, 12));
            }

            if (!deterministicallyExited.contains(childID))
            {
                // draw little box to the right of the node displaying
                // determinism information
                FontMetrics fm = canvas.getFontMetrics();
                int width = fm.stringWidth(n.tag);
                int x = n.x + width / 2 + 6;
                if (panel.getNodePositioning() == TreeViewPanel.LEFT_ALIGNMENT)
                {
                    x = n.x + width + 6;
                }
                x += panel.t.getIndent(childID);
                int y = n.y - 10;
                canvas.setColor(canvas.getBackground());
                canvas.fillRect(x - 4, y, 16, 12);

                canvas.setColor(Color.BLACK);
                canvas.drawRect(x - 4, y, 4, 12);

                canvas.drawRect(x, y, 12, 12);

                canvas.setColor(Color.ORANGE);
                if (nonDetermBecauseOfRedo.contains(childID))
                {
                    canvas.setColor(Color.DARK_GRAY);
                }
                canvas.fillRect(x + 1, y + 1, 11, 11);
            }
            canvas.setColor(Color.BLACK);
        }
    }
}
