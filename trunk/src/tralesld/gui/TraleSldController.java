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
        String s = event.getActionCommand();
        if (s.equals(""))
        {
            
        }
    }
    
    public void itemStateChanged(ItemEvent e) 
    {
        System.err.println("Checkbox change!");
        JCheckBox source = (JCheckBox) e.getItemSelectable();
        System.err.println("Checkbox change!");
        if (source.getText().equals("show junk edges")) 
        {
            boolean junkEdges = source.isSelected();
            gui.cvp.v = ChartViewBuilder.buildChartView(sld.curCM, junkEdges);  
        }    
    }

}
