import java.awt.event.*;
import javax.swing.*;
import java.util.*;

public class LCParserGUI extends JFrame implements ActionListener, WindowListener
{
	//serialVersionUID to avoid warning
	private static final long serialVersionUID = 1L;
	
	TreeViewPanel treePanel1;
	JScrollPane treePanelScrollPane;
	
	JLabel currentStepLabel;
	JLabel displayStepLabel;
	JLabel stepDescriptionLabel;
	
	JButton nextStepButton;
	JButton previousStepButton;
	
	JButton acceptButton;
	JButton rejectButton;
	JButton autoCompleteButton;
	JButton abortButton;
	
	ArrayList<String> parseSteps;
	ArrayList<String> parseStepDescriptions;
	int currentDisplayStep;
	
	ArrayList<TreeView> parseStepViews;
	
	String pressedButton;
    
    //window activation modes; determine which buttons are enabled
    static final int START = 0;
    static final int FIRST_IN_HISTORY = 1;
    static final int IN_HISTORY = 2;
    static final int LAST_IN_HISTORY = 3;
    static final int ABORTED = 4;
    static final int PARSING_COMPLETE = 5;
	
	public LCParserGUI()
	{	
		parseSteps = new ArrayList<String>();
		parseStepDescriptions = new ArrayList<String>();
		currentDisplayStep = -1;
		
		parseStepViews = new ArrayList<TreeView>();
		
		pressedButton = "none";
		
		treePanel1 = new TreeViewPanel();
		treePanelScrollPane = new JScrollPane(treePanel1,JScrollPane.VERTICAL_SCROLLBAR_ALWAYS, JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
		treePanelScrollPane.setBounds(20,50,600,400);
		
		currentStepLabel = new JLabel("Step   " + (currentDisplayStep + 1));
		currentStepLabel.setBounds(20, 20, 80, 20);
		
		displayStepLabel = new JLabel(" of " + parseSteps.size());
		displayStepLabel.setBounds(80, 20, 80, 20);
		
		stepDescriptionLabel = new JLabel("No parse possible!");
		stepDescriptionLabel.setBounds(20,470,600,60);
		
		previousStepButton = new JButton("Previous");
		previousStepButton.setBounds(150, 20, 100, 20);
		previousStepButton.addActionListener(this);
		
		nextStepButton = new JButton("Next");
		nextStepButton.setBounds(250, 20, 100, 20);
		nextStepButton.addActionListener(this);
		
		acceptButton = new JButton("Accept");
		acceptButton.setBounds(630, 50, 150, 20);
		acceptButton.addActionListener(this);
		
		rejectButton = new JButton("Reject");
		rejectButton.setBounds(630, 80, 150, 20);
		rejectButton.addActionListener(this);
		
		autoCompleteButton = new JButton("Auto Complete");
		autoCompleteButton.setBounds(630, 140, 150, 20);
		autoCompleteButton.addActionListener(this);
		
		abortButton = new JButton("Abort");
		abortButton.setBounds(630, 170, 150, 20);
		abortButton.addActionListener(this);
		
		this.getContentPane().setLayout(null);
		this.getContentPane().add(treePanelScrollPane);
		this.getContentPane().add(currentStepLabel);
		this.getContentPane().add(displayStepLabel);
		this.getContentPane().add(stepDescriptionLabel);
		this.getContentPane().add(previousStepButton);
		this.getContentPane().add(nextStepButton);
		this.getContentPane().add(acceptButton);
		this.getContentPane().add(rejectButton);
		this.getContentPane().add(autoCompleteButton);
		this.getContentPane().add(abortButton);
		this.setSize(800,600);
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
		treePanel1.displayTreeView(tv);
	}
	
	public void addParseStepToHistory(String prologList)
	{
		parseSteps.add(prologList);
		TreeModel tm = TreeModelFactory.createTreeModelFromPrologList(prologList);
		TreeView tv = new TreeView(tm);
		parseStepViews.add(tv);
		
		if (parseStepDescriptions.size() < parseSteps.size())
		{
			parseStepDescriptions.add("No description available!");
		}
		currentDisplayStep = parseSteps.size() - 1;
		if (!pressedButton.equals("auto_complete"))
		{
			pressedButton = "none";
		}

		updateTreePanelDisplay();
	}
	
	public void addDescriptionForNextParseStep(String description)
	{
		parseStepDescriptions.add(description.replaceAll("\n", "<br>"));
	}
	
	public void updateTreePanelDisplay()
	{
		currentStepLabel.setText("Step   " + (currentDisplayStep + 1));	
		displayStepLabel.setText(" of " + parseSteps.size());
		
        if (currentDisplayStep >= 0)
        {
    		String descriptionText = parseStepDescriptions.get(currentDisplayStep);
    		if (descriptionText == null)
    		{
    			descriptionText = "No description available!";
    		}
    		stepDescriptionLabel.setText("<html>" + descriptionText + "</html>");
		
			treePanel1.displayTreeView(parseStepViews.get(currentDisplayStep));
		}
        
        setButtonActivationMode(IN_HISTORY);
        if (currentDisplayStep == 0)
        {
            setButtonActivationMode(FIRST_IN_HISTORY);
        }
        if (currentDisplayStep == parseSteps.size() - 1)
        {
            setButtonActivationMode(LAST_IN_HISTORY);
        }
	}
    
    public void parsingSuccess()
    {
        setButtonActivationMode(PARSING_COMPLETE);
    }
    
    public void parsingFailure()
    {
        setButtonActivationMode(PARSING_COMPLETE);
    }
	
	public void displayNextStep()
	{	
		if (currentDisplayStep < parseSteps.size() - 1)
		{
			currentDisplayStep++;
			updateTreePanelDisplay();
		}
	}
	
	public void displayPreviousStep()
	{	
		if (currentDisplayStep > 0)
		{
			currentDisplayStep--;
			updateTreePanelDisplay();
		}
	}
	
	public void actionPerformed(ActionEvent e)
	{
		String cmd = e.getActionCommand();
		if (cmd.equals("Next"))
		{
			pressedButton = "next";
			displayNextStep();
		}
		else if (cmd.equals("Previous"))
		{
			pressedButton = "previous";
			displayPreviousStep();
		}
		else if (cmd.equals("Accept"))
		{
			pressedButton = "accept";
			displayPreviousStep();
		}
		else if (cmd.equals("Reject"))
		{
			pressedButton = "reject";
            currentDisplayStep--;
            updateTreePanelDisplay();
            parseSteps.remove(parseSteps.size() - 1);
            parseStepDescriptions.remove(parseSteps.size() - 1);
            parseStepViews.remove(parseStepViews.size() - 1);
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
                acceptButton.setEnabled(true);
                rejectButton.setEnabled(true);
                autoCompleteButton.setEnabled(true);
                abortButton.setEnabled(true);
                break;
            }
            case FIRST_IN_HISTORY:
            {
                previousStepButton.setEnabled(false);
                break;
            }
            case IN_HISTORY:
            {
                previousStepButton.setEnabled(true);
                nextStepButton.setEnabled(true);
                break;
            }
            case LAST_IN_HISTORY:
            {
                nextStepButton.setEnabled(false);
                break;
            }
            case ABORTED:
            {
                acceptButton.setEnabled(false);
                rejectButton.setEnabled(false);
                autoCompleteButton.setEnabled(false);
                abortButton.setEnabled(false);
                break;
            }
            case PARSING_COMPLETE:
            {
                acceptButton.setEnabled(false);
                rejectButton.setEnabled(false);
                autoCompleteButton.setEnabled(false);
                abortButton.setEnabled(false);
                break;
            }
        }
    }
	
	
	public static void main(String[] args)
	{
		LCParserGUI gui = new LCParserGUI();
		gui.addParseStepToHistory("[s,[[*d,[the]]]]");
		gui.addParseStepToHistory("[d, [the]]");
		gui.addParseStepToHistory("[s, [[np, [[d, [the]], [n, [dog]]]], [vp, [[v, [chases]], [np, [[np, [[d, [the]], [n, [elephant]]]], [conj, _G878], [np, _G887]]], [pp, _G614]]]]]");
		gui.addParseStepToHistory("[cp, [[spec_cp, [[pp, [[p, [wegen]], [dp, [[d, [ihrer]], [np, [[n, [diaet]]]]]]]]]], [cbar, [[c0, [hatte]], [ip, [[vp, [[spec_vp, [[dp, [[d, [die]], [np, [[n, [graefin]]]]]], [vbar, [[advp, [leider]], [dp, [[d, [keine]], [np, [[n, [austern]]]]]], [vbar, [[v, [nehmen]], [v, [duerfen]]]]]]]]]]]]]]]]");
        gui.addParseStepToHistory("[s,_X345]");
		gui.setVisible(true);
	}
}
