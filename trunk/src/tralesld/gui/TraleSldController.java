package tralesld.gui;

import tralesld.*;
import tralesld.visual.chart.ChartViewBuilder;
import java.awt.event.*;
import javax.swing.*;

public class TraleSldController implements ActionListener, ItemListener
{
    TraleSld sld;
    TraleSldGui gui;
    
    public TraleSldController(TraleSld sld)
    {
        this.sld = sld;
    }
    
    public void actionPerformed(ActionEvent event)
    {
		String cmd = event.getActionCommand();
		if (cmd.equals("c"))
		{
			sld.reply = 'c';
		}
		else if (cmd.equals("f"))
		{
            //gui.nodeColorings.put(sld.currentDecisionTreeNode, Color.RED);
			sld.reply = 'f';         
		}
		else if (cmd.equals("s"))
		{
			sld.reply = 's';
		}
		/*else if (cmd.equals("Abort"))
		{
		    setButtonActivationMode(ABORTED);
			pressedButton = "abort";
		}*/
        //updateTreePanelDisplay();
    }
    
    public void itemStateChanged(ItemEvent e) 
    {
        JCheckBox source = (JCheckBox) e.getItemSelectable();
        if (source.getText().equals("show junk edges")) 
        {     
            boolean junkEdges = source.isSelected();
            gui.cvp.displayFailedEdges = junkEdges;
            gui.updateChartPanelDisplay();
        }    
    }

}
