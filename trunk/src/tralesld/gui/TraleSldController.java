package tralesld.gui;

import tralesld.*;
import tralesld.struct.chart.ChartEdge;
import java.awt.event.*;
import java.util.LinkedList;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.DefaultMutableTreeNode;

public class TraleSldController implements ActionListener, ItemListener, TreeSelectionListener
{
    TraleSld sld;
    TraleSldGui gui;
    
    boolean ignoreNextOverviewChange;
    
    public TraleSldController(TraleSld sld)
    {
        this.sld = sld;
        this.gui = null;
        this.ignoreNextOverviewChange = false;
    }
    
    public void setGUI(final TraleSldGui gui)
    {
    	this.gui = gui;
    	InputMap globalInputMap = gui.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
    	globalInputMap.put(KeyStroke.getKeyStroke('c'), "creep");
    	globalInputMap.put(KeyStroke.getKeyStroke('s'), "skip");
    	globalInputMap.put(KeyStroke.getKeyStroke('f'), "fail");
    	globalInputMap.put(KeyStroke.getKeyStroke('l'), "leap");
    	globalInputMap.put(KeyStroke.getKeyStroke("alt C"), "showChart");
    	globalInputMap.put(KeyStroke.getKeyStroke("alt T"), "showTree");
    	globalInputMap.put(KeyStroke.getKeyStroke("alt S"), "showSignature");
    	globalInputMap.put(KeyStroke.getKeyStroke("alt N"), "showConstraints");
    	globalInputMap.put(KeyStroke.getKeyStroke("alt R"), "showRules");

    	ActionMap actionMap = gui.getActionMap();
    	actionMap.put("creep", new AbstractAction() 
    	{
    	    @Override
    	    public void actionPerformed(ActionEvent e) 
    	    {
    	    	sld.reply = 'c';         
    	    }
    	});
    	actionMap.put("skip", new AbstractAction() 
    	{
    	    @Override
    	    public void actionPerformed(ActionEvent e) 
    	    {
    	    	sld.reply = 's';
                sld.skipToStep = sld.currentDecisionTreeNode;
                System.err.println("Skipping to next occurrence of stepID " + sld.skipToStep);        
    	    }
    	});
    	actionMap.put("fail", new AbstractAction() 
    	{
    	    @Override
    	    public void actionPerformed(ActionEvent e) 
    	    {
    			sld.reply = 'f';      
    	    }
    	});
    	actionMap.put("leap", new AbstractAction() 
    	{
    	    @Override
    	    public void actionPerformed(ActionEvent e) 
    	    {
    	    	sld.reply = 'l';        
    	    }
    	});
    	actionMap.put("showChart", new AbstractAction() 
    	{
    	    @Override
    	    public void actionPerformed(ActionEvent e) 
    	    {
    	    	gui.chartPanel.setSelectedIndex(0);       
    	    }
    	});
    	actionMap.put("showTree", new AbstractAction() 
    	{
    	    @Override
    	    public void actionPerformed(ActionEvent e) 
    	    {
    	    	gui.chartPanel.setSelectedIndex(1);       
    	    }
    	});
    	actionMap.put("showSignature", new AbstractAction() 
    	{
    	    @Override
    	    public void actionPerformed(ActionEvent e) 
    	    {
    	    	gui.grammarPanel.setSelectedIndex(0);       
    	    }
    	});
    	actionMap.put("showConstraints", new AbstractAction() 
    	{
    	    @Override
    	    public void actionPerformed(ActionEvent e) 
    	    {
    	    	gui.grammarPanel.setSelectedIndex(1);       
    	    }
    	});
    	actionMap.put("showRules", new AbstractAction() 
    	{
    	    @Override
    	    public void actionPerformed(ActionEvent e) 
    	    {
    	    	gui.grammarPanel.setSelectedIndex(2);       
    	    }
    	});
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
			sld.reply = 'f';         
		}
		else if (cmd.equals("s"))
		{
			sld.reply = 's';
            sld.skipToStep = sld.currentDecisionTreeNode;
            System.err.println("Skipping to next occurrence of stepID " + sld.skipToStep);
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
