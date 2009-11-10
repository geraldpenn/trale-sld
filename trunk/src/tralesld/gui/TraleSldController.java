package tralesld.gui;

import tralesld.*;
import tralesld.struct.chart.ChartEdge;

import java.awt.Component;
import java.awt.event.*;
import java.util.LinkedList;
import javax.swing.*;
import javax.swing.event.*;
import javax.swing.tree.DefaultMutableTreeNode;

public class TraleSldController implements ActionListener, ItemListener, MouseListener, WindowListener
{
    TraleSld sld;
    TraleSldGui gui;
    
    JFrame sourceFrame;
    JFrame chartFrame;
    JFrame localTreeFrame;
    JFrame controlFlowFrame;
    JFrame variablesFrame;
    JFrame signatureFrame;
    
    MouseEvent lastMouseEvent;
    
    public TraleSldController(TraleSld sld)
    {
        this.sld = sld;
        this.gui = null;
        this.sourceFrame = null;
        this.chartFrame = null;
        this.localTreeFrame = null;
        this.controlFlowFrame = null;
        this.variablesFrame = null;
        this.signatureFrame = null;
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
    
    public void valueChanged(TreeSelectionEvent e)
    {   
        

    }

	@Override
	public void mouseClicked(MouseEvent e) 
	{
	    Object o = e.getSource();
	    boolean doubleClick = false;
	    //e.getClickCount() not reliable on all architectures!
	    if (lastMouseEvent != null && e.getWhen() - lastMouseEvent.getWhen() < 500)
	    {
	        doubleClick = true;
	    }
	    else
	    {
	        lastMouseEvent = e;
	    }   
		if (o == gui.chartButton)
		{
		    if (doubleClick)
		    {
    			chartFrame = new JFrame("Chart");
    		    chartFrame.add((JPanel) gui.chartTab);
    		    chartFrame.addWindowListener(this);
    
    	        // Display the window.
    	        chartFrame.pack();
    	        chartFrame.setVisible(true);
		    }
		    else
		    {
		        gui.chartPanel.setSelectedComponent(gui.chartTab);
		    }
		}
		else if (o == gui.sourceButton)
		{
            if (doubleClick)
            {
    			sourceFrame = new JFrame("Source display");
    			sourceFrame.add(gui.sourcePanel);
    			sourceFrame.addWindowListener(this);
    			
    	        // Display the window.
    	        sourceFrame.pack();
    	        sourceFrame.setVisible(true);
            }
            else
            {
                gui.grammarPanel.setSelectedComponent(gui.sourcePanel);
            }
		}
		else if (o == gui.signatureButton)
		{
            if (doubleClick)
            {
    			signatureFrame = new JFrame("Signature");
    			signatureFrame.add(gui.signaturePanel);
    			signatureFrame.addWindowListener(this);
    			
    	        // Display the window.
    	        signatureFrame.pack();
    	        signatureFrame.setVisible(true);
            }
            else
            {
                gui.grammarPanel.setSelectedComponent(gui.signaturePanel);
            }
		}
		else if (o == gui.variableButton)
		{
            if (doubleClick)
            {
    			variablesFrame = new JFrame("Variables");
    			variablesFrame.add(gui.variablesPanel);
    			variablesFrame.addWindowListener(this);
    			
    	        // Display the window.
    	        variablesFrame.pack();
    	        variablesFrame.setVisible(true);
            }
            else
            {
                gui.detailPanel.setSelectedComponent(gui.variablesPanel);
            }
		}
		else if (o == gui.controlFlowButton)
		{
		    if (doubleClick)
		    {
    			controlFlowFrame = new JFrame("Control flow");
    			controlFlowFrame.add(gui.controlFlowTab);
    			controlFlowFrame.addWindowListener(this);
    			
    	        // Display the window.
    	        controlFlowFrame.pack();
    	        controlFlowFrame.setVisible(true);
		    }
            else
            {
                gui.chartPanel.setSelectedComponent(gui.controlFlowTab);
            }
		}
		else if (o == gui.stepDetailButton)
		{
	        if (doubleClick)
	        {
    			localTreeFrame = new JFrame("Local tree");
    			localTreeFrame.add(gui.localTreePanel);
    			gui.detailPanel.remove(gui.stepDetailTab);
    			localTreeFrame.addWindowListener(this);
    			
    	        // Display the window.
    	        localTreeFrame.pack();
    	        localTreeFrame.setVisible(true);
	        }
	        else
	        {
	            gui.detailPanel.setSelectedComponent(gui.stepDetailTab);
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
			gui.chartPanel.addTab("Control flow graph", gui.controlFlowTab);
		}
		
	}

	@Override
	public void windowClosing(WindowEvent e) 
	{
		// TODO Auto-generated method stub
		Object source = e.getSource();
		if (source == controlFlowFrame)
		{
			gui.chartPanel.addTab("Control flow graph", gui.controlFlowTab);
	        gui.chartPanel.setTabComponentAt(gui.chartPanel.getTabCount() - 1, gui.controlFlowButton);
		}
		else if (source == chartFrame)
		{
			gui.chartPanel.addTab("Chart", gui.chartTab);
	        gui.chartPanel.setTabComponentAt(gui.chartPanel.getTabCount() - 1, gui.chartButton);
		}
		else if (source == variablesFrame)
        {
            gui.detailPanel.addTab("Variables", gui.variablesPanel);
            gui.detailPanel.setTabComponentAt(gui.detailPanel.getTabCount() - 1, gui.variableButton);
        }
        else if (source == signatureFrame)
        {
            gui.grammarPanel.addTab("Signature", gui.signaturePanel);
            gui.grammarPanel.setTabComponentAt(gui.grammarPanel.getTabCount() - 1, gui.signatureButton);
        }
        else if (source == sourceFrame)
        {
            gui.grammarPanel.addTab("Source", gui.sourcePanel);
            gui.grammarPanel.setTabComponentAt(gui.grammarPanel.getTabCount() - 1, gui.sourceButton);
        }
        else if (source == localTreeFrame)
        {
            gui.detailPanel.addTab("Local tree", gui.createLocalTreeTab(gui.localTreePanel));
            gui.detailPanel.setTabComponentAt(gui.detailPanel.getTabCount() - 1, gui.stepDetailButton);
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
