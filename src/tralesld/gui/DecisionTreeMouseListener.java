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
    	
    	TreeViewNode candidateNode = viewPanel.t.treeNodes.get(viewPanel.t.rootID);
    	while (candidateNode.y < y && candidateNode.children.size() > 0)
    	{
    		List<Integer> children = candidateNode.children;
    		candidateNode = viewPanel.t.treeNodes.get(children.get(0));
    		for (int i : children)
    		{
    			if (viewPanel.t.treeNodes.get(i).x < x)
    			{
    				candidateNode = viewPanel.t.treeNodes.get(i);
    			}
    		}
    	}
        gui.decisionTreeNodeClick(candidateNode.id);
        /*String event = "c" + e.getX() + "/" + e.getY();
        if (viewPanel.eventGrid.get(event) != null)
        {
            int selectedNode = viewPanel.eventGrid.get(event);
            gui.decisionTreeNodeClick(selectedNode);
        }
        else
        {
            if (e.getButton() != MouseEvent.BUTTON1)
            {
                // popup: to be reactivated
                // display.panePopup.show(this,e.getX(),e.getY());
            }
        }*/
    }
}
