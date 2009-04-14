package tralesld.gui;
import java.awt.event.MouseEvent;
import java.util.List;

import tralesld.visual.tree.*;


public class DecisionTreeMouseListener extends TreeViewMouseListener
{
    TraleSldGui gui;
    
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
        gui.decisionTreeNodeClick(nodeID);
    }
}
