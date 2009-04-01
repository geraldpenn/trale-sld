package tralesld.gui;

import gralej.parsers.ParseException;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Point;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import javax.swing.*;
import javax.swing.tree.*;

import tralesld.*;
import tralesld.gui.icons.IconUtil;
import tralesld.storage.DataStore;
import tralesld.storage.DataStore;
import tralesld.struct.chart.*;
import tralesld.struct.tree.*;
import tralesld.util.VisualizationUtility;
import tralesld.visual.chart.*;
import tralesld.visual.tree.*;

public class TraleSldGui extends JPanel
{
    TraleSld sld;
    TraleSldController ctrl;
    
    VisualizationUtility util;
    
    ChartViewPanel cvp;
    //decision tree panel
    public TreeViewPanel dtp;
    JScrollPane dtvsp;
    
    //step detail panel (feature structures etc.)
    JPanel stepDetailPanel;
    
    public JTree overviewTree;
    DefaultTreeModel overviewTreeModel;
    DefaultMutableTreeNode overviewTreeRoot;
    
    //map chart edges via their IDs to associated overview tree nodes
    public DataStore<DefaultMutableTreeNode> stepRegister;
    
    public int traceNodeID;
    public HashMap<Integer,Color> nodeColorings;
    
    //register active chart edges for proper redrawing
    public List<ChartEdge> activeChartEdges;

    public TraleSldGui(TraleSldController ctrl)
    {
        super(new GridLayout(1, 0));
        this.ctrl = ctrl;
        add(createVerticalSplit());
        ctrl.gui = this;  
        
        util = new VisualizationUtility();
        
    	stepRegister = new DataStore<DefaultMutableTreeNode>();
    	
        traceNodeID = 0;
        nodeColorings = new HashMap<Integer,Color>();
        activeChartEdges = new LinkedList<ChartEdge>();
    }

    private JComponent createVerticalSplit()
    {
        JSplitPane result = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, createLeftPanel(), createRightPanel());
        result.setResizeWeight(1);
        return result;
    }
    
    private JComponent createLeftPanel()
    {
        JSplitPane result = new JSplitPane(JSplitPane.VERTICAL_SPLIT, createControlPanel(), createGrammarPanel());
        result.setPreferredSize(new Dimension(224, 768));
        return result;
    }

    private JComponent createRightPanel()
    {
        JSplitPane result = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, createChartPanel(), createStepDetailPanel());
        result.setPreferredSize(new Dimension(800, 768));
        result.setResizeWeight(1);
        return result;
    }

    private JComponent createStepDetailPanel()
    {
        JTabbedPane result = new JTabbedPane();
        result.addTab("apply head_subject rule", createStepDetailTab());
        return result;
    }

    private JComponent createChartPanel()
    {
        JTabbedPane result = new JTabbedPane();
        result.setPreferredSize(new Dimension(800, 300));
        result.addTab("Chart", createChartTab());
        result.addTab("DecisionTree", createDecisionTreeTab());
        return result;
    }

    private JComponent createControlPanel()
    {
        JTabbedPane result = new JTabbedPane();
        result.setPreferredSize(new Dimension(224, 384));
        result.addTab("Control", createControlTab());
        return result;
    }

    private JComponent createGrammarPanel()
    {
        JTabbedPane result = new JTabbedPane();
        result.addTab("Signature", createSignatureTab());
        result.addTab("Constraints", createConstraintsTab());
        result.addTab("Rules", createRulesTab());
        return result;
    }

    private JComponent createStepDetailTab()
    {
        stepDetailPanel = new JPanel();
        return stepDetailPanel;
    }

    private JComponent createChartTab()
    {
        JPanel result = new JPanel();
        result.setLayout(new BoxLayout(result, BoxLayout.Y_AXIS));
        result.add(createChartControlPanel());

        cvp = new ChartViewPanel();
        ChartViewMouseListener traleChartMouseListener = new TraleChartMouseListener(cvp,this);
        cvp.addMouseListener(traleChartMouseListener);
        cvp.v = new ChartView();

        JScrollPane scrollPane = new JScrollPane(cvp);
        scrollPane.setBackground(Color.WHITE);
        result.add(scrollPane);
        return result;
    }
    
    private JComponent createDecisionTreeTab()
    {
        JPanel result = new JPanel();
        result.setLayout(new BoxLayout(result, BoxLayout.Y_AXIS));
        result.add(createTreeControlPanel());


        dtp = new TreeViewPanel();
        dtp.t = new TreeView();
        DecisionTreeMouseListener decTreeMouseListener = new DecisionTreeMouseListener(dtp,this);
        dtp.addMouseListener(decTreeMouseListener);
        dtp.edgyLines = false;

        dtvsp = new JScrollPane(dtp);
        dtvsp.setBackground(Color.WHITE);
        result.add(dtvsp);
        return result;
    }

    private JComponent createControlTab()
    {
        JPanel result = new JPanel();
        result.setLayout(new BoxLayout(result, BoxLayout.Y_AXIS));
        result.add(createControlButtonsPanel());
        result.add(createStepsTreePane());
        return result;
    }

    private JComponent createSignatureTab()
    {
        JPanel result = new JPanel();
        return result;
    }

    private JComponent createConstraintsTab()
    {
        JPanel result = new JPanel();
        return result;
    }

    private JComponent createRulesTab()
    {
        JPanel result = new JPanel();
        return result;
    }

    private JComponent createChartControlPanel()
    {
        JPanel result = new JPanel();
        result.setLayout(new BoxLayout(result, BoxLayout.X_AXIS));
        JCheckBox junkEdgeCheckBox = new JCheckBox("show junk edges", true);
        junkEdgeCheckBox.addItemListener(ctrl);
        result.add(junkEdgeCheckBox);
        result.add(Box.createHorizontalGlue());
        return result;
    }
    
    private JComponent createTreeControlPanel()
    {
        JPanel result = new JPanel();
        result.setLayout(new BoxLayout(result, BoxLayout.X_AXIS));
        return result;
    }

    private JComponent createControlButtonsPanel()
    {
        JPanel result = new JPanel();
        result.setLayout(new BoxLayout(result, BoxLayout.X_AXIS));
        JButton creepButton = createButton("creep.png", "continue");
        creepButton.addActionListener(ctrl);
        creepButton.setActionCommand("c");
        result.add(creepButton);
        result.add(Box.createHorizontalGlue());
        JButton skipButton = createButton("skip.png", "auto-complete this step");
        skipButton.addActionListener(ctrl);
        skipButton.setActionCommand("s");
        result.add(skipButton);
        result.add(Box.createHorizontalGlue());
        JButton rejectButton = createButton("reject.png", "make this step fail");
        rejectButton.addActionListener(ctrl);
        rejectButton.setActionCommand("f");
        result.add(rejectButton);
        result.add(Box.createHorizontalGlue());
        JButton leapButton = createButton("leap.png", "auto-complete");
        leapButton.addActionListener(ctrl);
        leapButton.setActionCommand("l");
        result.add(leapButton);
        result.add(Box.createHorizontalGlue());
        return result;
    }

    private JComponent createStepsTreePane()
    {
        return new JScrollPane(createStepsTreeView());
    }

    private JComponent createStepsTreeView()
    {
        overviewTreeRoot = createTreeNode(new Step(Step.STATUS_PROGRESS, "parsing", 0));
        overviewTreeModel = new DefaultTreeModel(overviewTreeRoot);
        overviewTree = new JTree(overviewTreeModel);
        overviewTree.setEditable(true);
        overviewTree.setCellRenderer(new StepRenderer());
        overviewTree.addTreeSelectionListener(ctrl);
        ctrl.ignoreNextOverviewTreeSelectionChange();
        TreePath selectionPath = new TreePath(overviewTreeRoot.getPath());
        overviewTree.setSelectionPath(selectionPath);
        return overviewTree;
    }

    private JButton createButton(String filename, String toolTipText)
    {
        JButton result = new JButton(new ImageIcon(IconUtil.getIcon(filename)));
        result.setToolTipText(toolTipText);
        return result;
    }

    private DefaultMutableTreeNode createTreeNode(Object userObject)
    {
        return createTreeNode(userObject, new DefaultMutableTreeNode[0]);
    }

    private DefaultMutableTreeNode createTreeNode(Object userObject, DefaultMutableTreeNode[] children)
    {
        DefaultMutableTreeNode result = new DefaultMutableTreeNode(userObject);

        for (DefaultMutableTreeNode child : children)
        {
            result.add(child);
        }

        return result;
    }

    private class StepRenderer extends DefaultTreeCellRenderer
    {

        /**
         * 
         */
        private static final long serialVersionUID = 8045182470829809316L;

        private Icon progressIcon = new ImageIcon(IconUtil.getIcon("progress.png"));

        private Icon successIcon = new ImageIcon(IconUtil.getIcon("success.png"));

        private Icon failureIcon = new ImageIcon(IconUtil.getIcon("failure.png"));

        public JComponent getTreeCellRendererComponent(JTree tree, Object value, boolean sel, boolean expanded, boolean leaf, int row, boolean hasFocus)
        {
            super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);

            if (!(value instanceof DefaultMutableTreeNode))
            {
                return this;
            }

            Object userObject = ((DefaultMutableTreeNode) value).getUserObject();

            if (!(userObject instanceof Step))
            {
                return this;
            }

            Step step = (Step) userObject;
            int status = step.getStatus();

            if (status == Step.STATUS_PROGRESS)
            {
                setIcon(progressIcon);
            }
            else if (status == Step.STATUS_SUCCESS)
            {
                setIcon(successIcon);
            }
            else if (status == Step.STATUS_FAILURE)
            {
                setIcon(failureIcon);
            }

            setText(step.getText());

            return this;
        }
    }

    public static TraleSldGui createAndShowGUI(TraleSld sld)
    {
        // Create and set up the window.
        JFrame frame = new JFrame("TraleSLD");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        TraleSldController ctrl = new TraleSldController(sld);

        // Add content to the window.
        TraleSldGui gui = new TraleSldGui(ctrl);
        
        gui.sld = sld;    

        frame.add(gui);

        // Display the window.
        frame.pack();
        frame.setVisible(true);

        return gui;
    }
    
    public void decisionTreeNodeClick(int clickedNode)
    {
        traceNodeID = clickedNode;
        updateAllDisplays();
    }
    
    public void updateChartPanelDisplay()
	{	
    	sld.curCM = new ChartModel(sld.curCM.words);
    	for (int i = 0; i <= traceNodeID; i++)
    	{
    		List<ChartModelChange> cmcl = sld.chartChanges.getData(i);
    		if (cmcl != null)
    		{
    			for (ChartModelChange cmc : cmcl)
    			{
    				sld.curCM.processChange(cmc);
    			}
    		}
    	}
    	cvp.v = ChartViewBuilder.buildChartView(sld.curCM, cvp.displayFailedEdges);
    	cvp.repaint();
	}
    
    public void updateStepDetails()
    {
        stepDetailPanel.removeAll();
        if (sld.nodeData.getData(traceNodeID) != null)
        {
            String featDetail = sld.nodeData.getData(traceNodeID).get("call");
            if (featDetail != null)
            {
                try
                {
                    stepDetailPanel.add(util.visualize(featDetail));
                }
                catch (ParseException e)
                {
                    stepDetailPanel.add(new JLabel("Parse error: " + e.getMessage()));
                }
            }
            else
            {
                stepDetailPanel.add(new JLabel("No step details found under ID \"call\"!"));
            }
        }
        else
        {
            stepDetailPanel.add(new JLabel("No step details found!"));
        }
        this.repaint();
    }
    
    public void updateTreeOverview()
    {   
        //save selected path to be restored later
        TreePath selectionPath = overviewTree.getSelectionPath();
        //System.err.println("Rebuild with following selection path: \n" + selectionPathToString(selectionPath));
        
        //extend overview tree with information from new nodes
        ((Step) overviewTreeRoot.getUserObject()).setText(sld.tracer.overviewTraceModel.nodes.get(sld.tracer.overviewTraceModel.root).content);
        Enumeration childEnum = overviewTreeRoot.children();
        for (int nID : sld.tracer.overviewTraceModel.nodes.get(sld.tracer.overviewTraceModel.root).children)
        {
            if (!childEnum.hasMoreElements() || ((Step) ((DefaultMutableTreeNode) childEnum.nextElement()).getUserObject()).getStepID() != sld.tracer.overviewTraceModel.nodes.get(nID).id)
            {
                DefaultMutableTreeNode newNode = convertToTreeNode(sld.tracer.overviewTraceModel.nodes.get(nID));
                stepRegister.put(sld.edgeRegister.getData(((Step) newNode.getUserObject()).getStepID()).id, newNode);
                overviewTreeRoot.add(newNode);
            }
            else
            {
                adaptTreeNode(sld.tracer.overviewTraceModel.nodes.get(nID));
            }
        }
        overviewTreeModel = new DefaultTreeModel(overviewTreeRoot);
        overviewTree.setModel(overviewTreeModel);
     
        ctrl.ignoreNextOverviewTreeSelectionChange();
        overviewTree.setSelectionPath(selectionPath);
        overviewTree.scrollPathToVisible(selectionPath);
        overviewTree.repaint();
    }
    
    private DefaultMutableTreeNode convertToTreeNode(TreeModelNode m)
    {
        DefaultMutableTreeNode mtn = createTreeNode(new Step(sld.stepStatus.getData(m.id), m.content, m.id));
        for (int nID : m.children)
        {
            DefaultMutableTreeNode newNode = convertToTreeNode(sld.tracer.overviewTraceModel.nodes.get(nID));
            stepRegister.put(sld.edgeRegister.getData(((Step) newNode.getUserObject()).getStepID()).id, newNode);
            mtn.add(newNode);
        }
        return mtn;
    }
    
    private void adaptTreeNode(TreeModelNode m)
    {
        DefaultMutableTreeNode mtn = stepRegister.getData(sld.edgeRegister.getData(m.id).id);
        ((Step) mtn.getUserObject()).setStatus(sld.stepStatus.getData(m.id));
        Enumeration childEnum = mtn.children();
        for (int nID : m.children)
        {
            if (!childEnum.hasMoreElements() || ((Step) ((DefaultMutableTreeNode) childEnum.nextElement()).getUserObject()).getStepID() != sld.tracer.overviewTraceModel.nodes.get(nID).id)
            {
                DefaultMutableTreeNode newNode = convertToTreeNode(sld.tracer.overviewTraceModel.nodes.get(nID));
                stepRegister.put(sld.edgeRegister.getData(((Step) newNode.getUserObject()).getStepID()).id, newNode);
                mtn.add(newNode);
            }
            else
            {
                adaptTreeNode(sld.tracer.overviewTraceModel.nodes.get(nID));
            }
        }
    }
    
    public void updateTreePanelDisplay()
	{		      
        tralesld.struct.tree.TreeModel dtm = new DecisionTreeModelBuilder().createTreeModel(sld.currentDecisionTreeHead);
        TreeView dtv = new TreeView(dtm, 200, 20);
        dtv.nodeShape = TreeView.BOX_SHAPE;
        processColorMarkings(dtv);
        addNodeMarking(dtv,traceNodeID, Color.YELLOW);
        ((TreeViewPanel) dtp).displayTreeView(dtv);
        //viewport change, trying to center decision tree view on active node --> buggy!
        JViewport view = dtvsp.getViewport();
        Point p = new Point(dtv.treeNodes.get(traceNodeID).x - 200, dtv.treeNodes.get(traceNodeID).y - 200);
        view.setViewPosition(p);
	}
    
    public void updateAllDisplays()
    {
    	updateChartPanelDisplay();
        updateStepDetails();
    	updateTreeOverview();
    	updateTreePanelDisplay();
    }
    
    public void addNodeMarking(TreeView t, int nodeID, Color color)
    {
    	TreeViewNode n = t.treeNodes.get(nodeID);
    	if (n != null)
    	{
    		n.color = color;
    	}
    }
    
    private void processColorMarkings(TreeView t)
    {
        for (int i : nodeColorings.keySet())
        {
        	TreeViewNode n = t.treeNodes.get(i);
        	if (n != null)
        	{
        		n.color = nodeColorings.get(i);
        	}
        }
    }
    
    public void changeActiveChartEdges(List<ChartEdge> activeEdges)
    {  	
    	for (ChartEdge e : activeChartEdges)
    	{
    		e.active = false;
    	}
    	activeChartEdges = activeEdges;
    	for (ChartEdge e : activeChartEdges)
    	{
    		e.active = true;
    	}
    	updateChartPanelDisplay();
    }
    
    public void selectChartEdge(ChartEdge e)
    {
    	if (e != null)
    	{
    		LinkedList<ChartEdge> elist = new LinkedList<ChartEdge>();
    		elist.add(e);
    		changeActiveChartEdges(elist);
            
            TreePath oldSelectionPath = overviewTree.getSelectionPath();
            //System.err.println("Old " + selectionPathToString(oldSelectionPath));
	    	TreePath selectionPath = new TreePath(stepRegister.getData(e.id).getPath());
            //System.err.println("New " + selectionPathToString(selectionPath));
            if (oldSelectionPath == null || !selectionPath.isDescendant(oldSelectionPath))
            {
                //System.err.println("Changing selection path!");
                ctrl.ignoreNextOverviewTreeSelectionChange();
                overviewTree.scrollPathToVisible(selectionPath);
                overviewTree.setSelectionPath(selectionPath);
            }
    	}
        else
        {
            //System.out.println("Trying to select null edge!");
        }
    }
    
    public String stepRegisterToString()
    {
    	String result = "step register:\n";
    	for (int i : stepRegister.getKeySet())
    	{
    		result += i + ": " + ((Step) stepRegister.getData(i).getUserObject()) + "\n";
    	}
    	return result;
    }
    
    public String selectionPathToString(TreePath selectionPath)
    {
        if (selectionPath == null) return "null";
    	String result = "selection path:\n";
    	for (Object n : selectionPath.getPath())
    	{
    		Step step = (Step) ((DefaultMutableTreeNode) n).getUserObject();
    		result += step.toString() + "\n";
    	}
    	return result;
    }
}
