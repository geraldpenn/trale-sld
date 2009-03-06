import java.awt.event.*;
import java.awt.*;
import javax.swing.*;
import java.util.*;

public class LCParserGUI extends JFrame implements ActionListener, WindowListener
{
	//serialVersionUID to avoid warning
	private static final long serialVersionUID = 1L;
	
    TreeViewPanel decisionTreePanel;
    JScrollPane decisionTreePanelScrollPane;
    
	TreeViewPanel inspectionPanel;
	JScrollPane inspectionPanelScrollPane;
	
	JLabel stepDescriptionLabel;
	
	JButton continueButton;
	JButton rejectButton;
    JButton confirmButton;
	JButton autoCompleteButton;
	JButton abortButton;
	
	ParsingDataStore<TreeView> parseStepViews;
    ParsingDataStore<String> parseStepDescriptions;
    ParseTracer tracer;
    ParseTraceModel traceModel;
    
    HashMap<Integer,Color> nodeColorings;
    
    int traceNodeID;
	
	String pressedButton;
    
    //window activation modes; determine which buttons are enabled
    static final int START = 0;
    static final int ABORTED = 4;
    static final int PARSING_COMPLETE = 5;
	
	public LCParserGUI()
	{	
        parseStepViews = new ParsingDataStore<TreeView>();
		parseStepDescriptions = new ParsingDataStore<String>();
		tracer = new ParseTracer();
        traceModel = tracer.traceModel;
        
        nodeColorings = new HashMap<Integer,Color>();
        
        traceNodeID = 0;
		
		pressedButton = "none";
        
        decisionTreePanel = new TreeViewPanel();
        decisionTreePanelScrollPane = new JScrollPane(decisionTreePanel,JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
        decisionTreePanelScrollPane.setBounds(20,20,600,480);
        DecisionTreeMouseListener decTreeMouseListener = new DecisionTreeMouseListener(decisionTreePanel,this);
        decisionTreePanel.addMouseListener(decTreeMouseListener);
        decisionTreePanel.edgyLines = false;
		
		inspectionPanel = new TreeViewPanel();
		inspectionPanelScrollPane = new JScrollPane(inspectionPanel,JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		inspectionPanelScrollPane.setBounds(20,500,600,300);
        inspectionPanel.edgyLines = false;
			
		stepDescriptionLabel = new JLabel("No parse possible!");
		stepDescriptionLabel.setBounds(20,820,600,60);
			
		continueButton = new JButton("Continue");
		continueButton.setBounds(630, 50, 150, 20);
		continueButton.addActionListener(this);
		
		rejectButton = new JButton("Reject");
		rejectButton.setBounds(630, 80, 150, 20);
		rejectButton.addActionListener(this);
        
        confirmButton = new JButton("Confirm");
        confirmButton.setBounds(630, 110, 150, 20);
        confirmButton.addActionListener(this);
		
		autoCompleteButton = new JButton("Auto Complete");
		autoCompleteButton.setBounds(630, 140, 150, 20);
		autoCompleteButton.addActionListener(this);
		
		abortButton = new JButton("Abort");
		abortButton.setBounds(630, 170, 150, 20);
		abortButton.addActionListener(this);
		
		this.getContentPane().setLayout(null);
        this.getContentPane().add(decisionTreePanelScrollPane);
		this.getContentPane().add(inspectionPanelScrollPane);
		this.getContentPane().add(stepDescriptionLabel);
		this.getContentPane().add(continueButton);
		this.getContentPane().add(rejectButton);
        this.getContentPane().add(confirmButton);
		this.getContentPane().add(autoCompleteButton);
		this.getContentPane().add(abortButton);
		this.setSize(800,900);
		this.setTitle("LC Parser GUI");
        
        this.addWindowListener(this);
        this.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	}
	
	public String getPressedButton()
	{
		return pressedButton;
	}
	
	public void displayTreeFromPrologList(String prologList)
	{
		TreeModel tm = TreeModelFactory.createTreeModelFromPrologList(prologList);
		TreeView tv = new TreeView(tm);
		((TreeViewPanel) inspectionPanel).displayTreeView(tv);
	}
	
	public void addParseStepToHistory(String stackState, String stepDescription, String shortDescription, String callTrace)
	{
		TreeModel tm = TreeModelFactory.createTreeModelFromPrologList(stackState);
		TreeView tv = new TreeView(tm);
        tv.nodeShape = TreeView.BOX_SHAPE;
		int id = parseStepViews.storeNewData(tv);
        
        traceNodeID = tracer.registerStep(callTrace, id, shortDescription);
		
		parseStepDescriptions.storeNewData(stepDescription);

		if (!pressedButton.equals("auto_complete"))
		{
			pressedButton = "none";
		}
		updateTreePanelDisplay();
	}
	
	public void updateTreePanelDisplay()
	{		
        int contentID = ParseTraceNode.nodes.get(traceNodeID).content;
		String descriptionText = parseStepDescriptions.getData(contentID);
		if (descriptionText == null)
		{
			descriptionText = "No description available!";
		}
		stepDescriptionLabel.setText("<html>" + descriptionText + "</html>");
		
        ((TreeViewPanel) inspectionPanel).displayTreeView(parseStepViews.getData(contentID));
        
        TreeModel dtm = traceModel.getTreeModel();
        TreeView dtv = new TreeView(dtm, 200, 50);
        processColorMarkings(dtv);
        addNodeMarking(dtv,traceNodeID, Color.YELLOW);
        ((TreeViewPanel) decisionTreePanel).displayTreeView(dtv);
        //viewport change, trying to center decision tree view on active node --> buggy!
        JViewport view = decisionTreePanelScrollPane.getViewport();
        Point p = new Point(dtv.treeNodes.get(traceNodeID).x - 200,dtv.treeNodes.get(traceNodeID).y - 200);
        view.setViewPosition(p);
	}
    
    public void decisionTreeNodeClick(int clickedNode)
    {
        traceNodeID = clickedNode;
        updateTreePanelDisplay();
    }
    
    private void removeNodeMarking(TreeView t, int nodeID)
    {
        t.treeNodes.get(nodeID).color = null;
    }
    
    private void addNodeMarking(TreeView t, int nodeID, Color color)
    {
        t.treeNodes.get(nodeID).color = color;
    }
    
    private void processColorMarkings(TreeView t)
    {
        for (int i : nodeColorings.keySet())
        {
            t.treeNodes.get(i).color = nodeColorings.get(i);
        }
    }
    
    public void parsingSuccess()
    {
        setButtonActivationMode(PARSING_COMPLETE);
    }
    
    public void parsingFailure()
    {
        setButtonActivationMode(PARSING_COMPLETE);
        stepDescriptionLabel.setText("Parsing failed!");
    }
	
	public void actionPerformed(ActionEvent e)
	{
		String cmd = e.getActionCommand();
		if (cmd.equals("Next"))
		{
			pressedButton = "next";
		}
		else if (cmd.equals("Previous"))
		{
			pressedButton = "previous";
		}
		else if (cmd.equals("Continue"))
		{
			pressedButton = "continue";
		}
		else if (cmd.equals("Reject"))
		{
            nodeColorings.put(traceNodeID, Color.RED);
			pressedButton = "reject";         
		}
        else if (cmd.equals("Confirm"))
        {
            nodeColorings.put(traceNodeID, Color.GREEN);
            pressedButton = "confirm";
        }
		else if (cmd.equals("Auto Complete"))
		{
			pressedButton = "auto_complete";
		}
		else if (cmd.equals("Abort"))
		{
		    setButtonActivationMode(ABORTED);
			pressedButton = "abort";
		}
        updateTreePanelDisplay();
	}
    
    public void windowActivated(WindowEvent e)
    {
    }
    
    public void windowClosed(WindowEvent e)
    {
        pressedButton = "close_window";
    }
    
    public void windowClosing(WindowEvent e)
    {
        pressedButton = "close_window";
    }
    
    public void windowDeactivated(WindowEvent e)
    {
    }
    
    public void windowDeiconified(WindowEvent e)
    {
    }
    
    public void windowIconified(WindowEvent e)
    {
    }
    
    public void windowOpened(WindowEvent e)
    {
    }
    
    public void terminateExecution()
    {
        this.dispose();
    }
    
    public void setButtonActivationMode(int i)
    {
        switch (i)
        {
            case START:
            {
                continueButton.setEnabled(true);
                rejectButton.setEnabled(true);
                confirmButton.setEnabled(true);
                autoCompleteButton.setEnabled(true);
                abortButton.setEnabled(true);
                break;
            }
            case ABORTED:
            {
                continueButton.setEnabled(false);
                rejectButton.setEnabled(false);
                confirmButton.setEnabled(false);
                autoCompleteButton.setEnabled(false);
                abortButton.setEnabled(false);
                break;
            }
            case PARSING_COMPLETE:
            {
                continueButton.setEnabled(false);
                rejectButton.setEnabled(false);
                confirmButton.setEnabled(false);
                autoCompleteButton.setEnabled(false);
                abortButton.setEnabled(false);
                break;
            }
        }
    }
	
	
	public static void main(String[] args)
	{
		LCParserGUI gui = new LCParserGUI();
		gui.addParseStepToHistory("[s,[[*d,[the]]]]","initialization","init","[]");
		gui.addParseStepToHistory("[s, [[np, [[d, [the]], [n, [dog]]]], [vp, [[v, [chases]], [np, [[np, [[d, [the]], [n, [elephant]]]], [conj, _G878], [np, _G887]]], [pp, _G614]]]]]", "complete step", "complete 1","[1]");
        gui.addParseStepToHistory("[s,[[*d,[the]]]]","initialization","try 2","[2,1]");
        gui.addParseStepToHistory("[s, [[np, [[d, [the]], [n, [dog]]]], [vp, [[v, [chases]], [np, [[np, [[d, [the]], [n, [elephant]]]], [conj, _G878], [np, _G887]]], [pp, _G614]]]]]", "complete step", "try 3", "[3,1]");
		gui.setVisible(true);
	}
}
