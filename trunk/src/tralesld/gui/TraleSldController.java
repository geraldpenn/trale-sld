package tralesld.gui;

import tralesld.*;
import tralesld.struct.chart.ChartEdge;
import java.awt.event.*;
import java.util.LinkedList;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.DefaultMutableTreeNode;

public class TraleSldController implements ActionListener, ItemListener, TreeSelectionListener, MouseListener, WindowListener
{
    TraleSld sld;
    TraleSldGui gui;
    
    JFrame sourceFrame;
    JFrame chartFrame;
    JFrame detailFrame;
    JFrame controlFlowFrame;
    JFrame variablesFrame;
    JFrame signatureFrame;
    
    boolean ignoreNextOverviewChange;
    
    public TraleSldController(TraleSld sld)
    {
        this.sld = sld;
        this.gui = null;
        this.sourceFrame = null;
        this.chartFrame = null;
        this.detailFrame = null;
        this.controlFlowFrame = null;
        this.variablesFrame = null;
        this.signatureFrame = null;
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

	@Override
	public void mouseClicked(MouseEvent e) 
	{
		
		if (e.getClickCount() >= 2)
		{
			Object o = e.getSource();
			if (o == gui.chartButton)
			{
				chartFrame = new JFrame("TraleSLD chart");
			    chartFrame.add((JPanel) gui.chartTab);
			    chartFrame.addWindowListener(this);

		        // Display the window.
		        chartFrame.pack();
		        chartFrame.setVisible(true);
			}
			else if (o == gui.sourceButton)
			{
				sourceFrame = new JFrame("TraleSLD source display");
				sourceFrame.add(gui.sourcePanel);
				
		        // Display the window.
		        sourceFrame.pack();
		        sourceFrame.setVisible(true);
			}
			else if (o == gui.signatureButton)
			{
				signatureFrame = new JFrame("TraleSLD signature");
				signatureFrame.add(gui.signaturePanel);
				
		        // Display the window.
		        signatureFrame.pack();
		        signatureFrame.setVisible(true);
			}
			else if (o == gui.variableButton)
			{
				variablesFrame = new JFrame("TraleSLD variables");
				variablesFrame.add(gui.variablesPanel);
				
		        // Display the window.
		        variablesFrame.pack();
		        variablesFrame.setVisible(true);
			}
			else if (o == gui.controlFlowButton)
			{
				controlFlowFrame = new JFrame("TraleSLD control flow");
				controlFlowFrame.add(gui.controlFlowTab);
				controlFlowFrame.addWindowListener(this);
				
		        // Display the window.
		        controlFlowFrame.pack();
		        controlFlowFrame.setVisible(true);
			}
			else if (o == gui.stepDetailButton)
			{
				detailFrame = new JFrame("TraleSLD step detail");
				detailFrame.add(gui.stepDetailPanel);
				
		        // Display the window.
		        detailFrame.pack();
		        detailFrame.setVisible(true);
			}
		}
		
	}

	@Override
	public void mouseEntered(MouseEvent e) 
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseExited(MouseEvent e) 
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mousePressed(MouseEvent e) 
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void mouseReleased(MouseEvent e) 
	{
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowActivated(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowClosed(WindowEvent e) 
	{
		System.err.println("window closed!");
		Object source = e.getSource();
		if (source == controlFlowFrame)
		{
			gui.chartPanel.addTab("Control Flow Graph", gui.controlFlowTab);
		}
		
	}

	@Override
	public void windowClosing(WindowEvent e) 
	{
		// TODO Auto-generated method stub
		Object source = e.getSource();
		if (source == controlFlowFrame)
		{
			gui.chartPanel.addTab("Control Flow Graph", gui.controlFlowTab);
	        gui.chartPanel.setTabComponentAt(gui.chartPanel.getTabCount() - 1, gui.controlFlowButton);
		}
		else if (source == chartFrame)
		{
			gui.chartPanel.addTab("Chart", gui.chartTab);
	        gui.chartPanel.setTabComponentAt(gui.chartPanel.getTabCount() - 1, gui.chartButton);
		}
	}

	@Override
	public void windowDeactivated(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowDeiconified(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowIconified(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void windowOpened(WindowEvent e) {
		// TODO Auto-generated method stub
		
	}

}
