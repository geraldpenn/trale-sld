package tralesld.gui;
import java.awt.event.MouseEvent;
import java.util.List;

import tralesld.visual.tree.*;


public class DecisionTreeMouseListener extends TreeViewMouseListener
{
    TraleSldGui gui;
    MouseEvent lastMouseEvent;
    
    public DecisionTreeMouseListener(TreeViewPanel viewPanel, TraleSldGui gui)
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
            if (lastMouseEvent != null && e.getWhen() - lastMouseEvent.getWhen() < 500)
            {
                gui.decisionTreeNodeDblClick(nodeID);
            }
            else
            {
                gui.decisionTreeNodeClick(nodeID);
                lastMouseEvent = e;
            }
        }
    }
}
