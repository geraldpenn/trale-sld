package tralesld.gui;

import tralesld.*;
import tralesld.struct.chart.ChartEdge;
import tralesld.struct.trace.XMLTraceModel;
import java.awt.event.*;
import java.util.LinkedList;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;

public class TraleSldController implements ActionListener, ItemListener, TreeSelectionListener
{
    TraleSld sld;
    TraleSldGui gui;
    
    boolean ignoreNextOverviewChange;
    
    public TraleSldController(TraleSld sld)
    {
        this.sld = sld;
        this.ignoreNextOverviewChange = false;
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
        else if (cmd.equals("l"))
        {
            sld.reply = 'l';
        }
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
    
    public void ignoreNextOverviewTreeSelectionChange()
    {
        ignoreNextOverviewChange = true;
    }
    
    public void valueChanged(TreeSelectionEvent e)
    {   
        
        if (ignoreNextOverviewChange)
        {
            ignoreNextOverviewChange = false;
            //System.err.println("Ignored overview tree change event!");
            return;
        }
        
	    DefaultMutableTreeNode node = (DefaultMutableTreeNode) gui.overviewTree.getLastSelectedPathComponent();
        
	    if (node == null)
        {
           //System.err.println("Nothing selected in overview tree!");
           return;
        }

	    Object nodeInfo = node.getUserObject();
        Step step = (Step) nodeInfo;
        int stepID = step.getStepID();
        
        //adapt chart view to new selection
        LinkedList<ChartEdge> activeChartEdges = new LinkedList<ChartEdge>();
        ChartEdge rootEdge = sld.edgeRegister.getData(stepID);
        if (rootEdge != null) activeChartEdges.add(rootEdge);
        gui.changeActiveChartEdges(activeChartEdges);
        
        //adapt decision tree view to new selection
        sld.currentDecisionTreeHead = sld.traceNodes.getData(stepID);
        //System.err.println("current decision tree head: " + sld.currentDecisionTreeHead);
        gui.traceNodeID = stepID;
        gui.updateChartPanelDisplay();
        gui.updateTreePanelDisplay();
    }

}
