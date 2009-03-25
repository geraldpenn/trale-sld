package tralesld.gui;
import java.awt.event.MouseEvent;

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
        String event = "c" + e.getX() + "/" + e.getY();
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
        }
    }
}
