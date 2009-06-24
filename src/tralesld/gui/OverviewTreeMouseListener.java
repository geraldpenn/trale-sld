package tralesld.gui;
import java.awt.event.MouseEvent;

import tralesld.visual.tree.*;

public class OverviewTreeMouseListener extends TreeViewMouseListener
{
    TraleSldGui gui;
    
    public OverviewTreeMouseListener(TreeViewPanel viewPanel, TraleSldGui gui)
    {
        super(viewPanel);
        this.gui = gui;
    }
    
    public void mouseClicked(MouseEvent e)
    {
        int x = e.getX();
        int y = e.getY();
        
        int nodeID = viewPanel.t.getNodeAtCoordinates(x,y);
        if (nodeID != -1)
        {
            if (e.getClickCount() > 1)
            {
                gui.overviewTreeNodeDblClick(nodeID);
            }
            else
            {
                gui.overviewTreeNodeClick(nodeID);
            }
        }
    }
}
