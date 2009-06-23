package tralesld.gui;

import java.awt.event.MouseEvent;

import tralesld.visual.chart.ChartViewMouseListener;
import tralesld.visual.chart.ChartViewPanel;

public class TraleChartMouseListener extends ChartViewMouseListener
{
	TraleSldGui gui;
    
    public TraleChartMouseListener(ChartViewPanel viewPanel, TraleSldGui gui)
    {
        super(viewPanel);
        this.gui = gui;
    }
    
    public void mouseClicked(MouseEvent e)
    {
        String event = e.getX() + "." + e.getY();
        
        if (viewPanel.eventGrid.get(event) != null)
        {
            int selectedEdge = viewPanel.eventGrid.get(event);
            System.err.println("selectedEdge: " + selectedEdge);
            gui.selectChartEdge(gui.sld.edgeRegister.getData(selectedEdge));
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
