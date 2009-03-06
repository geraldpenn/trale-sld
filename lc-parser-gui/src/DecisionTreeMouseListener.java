import java.awt.event.MouseEvent;


public class DecisionTreeMouseListener extends TreeViewMouseListener
{
    LCParserGUI gui;
    
    public DecisionTreeMouseListener(TreeViewPanel viewPanel, LCParserGUI gui)
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
