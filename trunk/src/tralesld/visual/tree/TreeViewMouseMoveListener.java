package tralesld.visual.tree;
import java.awt.event.*;

public class TreeViewMouseMoveListener extends TreeViewMouseListener implements MouseMotionListener
{
    int movedNodeID = -1;
    
    public TreeViewMouseMoveListener(TreeViewPanel viewPanel)
    {
        super(viewPanel);
    }
    
    public void mousePressed(MouseEvent e)
    {
    	String event = "c" + e.getX() + "/" + e.getY();
    	Integer nodeID = viewPanel.eventGrid.get(event);
    	if (nodeID != null)
    	{
    		movedNodeID = (Integer) nodeID;
    	}
    	viewPanel.repaint();
    }

    public void mouseReleased(MouseEvent e)
    {
        if (movedNodeID != -1)
        {
            TreeViewNode n = viewPanel.t.treeNodes.get(movedNodeID);
            n.x = e.getX();
            n.y = e.getY();
        }
        movedNodeID = -1;
    	viewPanel.repaint();
    }

    public void mouseEntered(MouseEvent e)
    {
    }

    public void mouseExited(MouseEvent e)
    {
    }

    public void mouseClicked(MouseEvent e)
    {
    }
    
    public void mouseMoved(MouseEvent e)
    {    
    }
    
    public void mouseDragged(MouseEvent e)
    {
        if (movedNodeID != -1)
        {
            TreeViewNode n = viewPanel.t.treeNodes.get(movedNodeID);
            n.x = e.getX();
            n.y = e.getY();
        }
    	viewPanel.repaint();
    }
}
