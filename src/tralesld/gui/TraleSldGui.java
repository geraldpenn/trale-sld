package tralesld.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Point;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.MutableTreeNode;

import tralesld.*;
import tralesld.gui.icons.IconUtil;
import tralesld.mockup.Step;
import tralesld.struct.chart.*;
import tralesld.struct.trace.XMLTraceNode;
import tralesld.struct.tree.*;
import tralesld.visual.chart.*;
import tralesld.visual.tree.*;

public class TraleSldGui extends JPanel
{
    TraleSld sld;
    TraleSldController ctrl;
    
    ChartViewPanel cvp;
    //decision tree panel
    TreeViewPanel dtp;
    JScrollPane dtvsp;
    
    public int traceNodeID;
    HashMap<Integer,Color> nodeColorings;

    public TraleSldGui(TraleSldController ctrl)
    {
        super(new GridLayout(1, 0));
        this.ctrl = ctrl;
        add(createVerticalSplit());
        ctrl.gui = this;     
        traceNodeID = 0;
        nodeColorings = new HashMap<Integer,Color>();
    }

    private JComponent createVerticalSplit()
    {
        JSplitPane result = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, createLeftPanel(), createRightPanel());
        result.setResizeWeight(1);
        return result;
    }

    private JComponent createLeftPanel()
    {
        JSplitPane result = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, createChartPanel(), createStepDetailPanel());
        result.setPreferredSize(new Dimension(800, 768));
        result.setResizeWeight(1);
        return result;
    }

    private JComponent createRightPanel()
    {
        JSplitPane result = new JSplitPane(JSplitPane.VERTICAL_SPLIT, createControlPanel(), createGrammarPanel());
        result.setPreferredSize(new Dimension(224, 768));
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
        return new JPanel(); // TODO
    }

    private JComponent createChartTab()
    {
        JPanel result = new JPanel();
        result.setLayout(new BoxLayout(result, BoxLayout.Y_AXIS));
        result.add(createChartControlPanel());


        cvp = new ChartViewPanel();
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
        MutableTreeNode root = createTreeNode(new Step(Step.STATUS_PROGRESS, "parse string \"it walks\""), new MutableTreeNode[] {
                createTreeNode(new Step(Step.STATUS_SUCCESS, "add edge 0")),
                createTreeNode(new Step(Step.STATUS_SUCCESS, "close edge 0"), new MutableTreeNode[] { createTreeNode(new Step(Step.STATUS_FAILURE, "apply head_complement rule")),
                        createTreeNode(new Step(Step.STATUS_FAILURE, "apply head_subject rule")) }),
                createTreeNode(new Step(Step.STATUS_SUCCESS, "add edge 1")),
                createTreeNode(new Step(Step.STATUS_SUCCESS, "close edge 1"), new MutableTreeNode[] {
                        createTreeNode(new Step(Step.STATUS_FAILURE, "apply head_complement rule")),
                        createTreeNode(new Step(Step.STATUS_SUCCESS, "apply head_subject rule"), new MutableTreeNode[] {
                                createTreeNode(new Step(Step.STATUS_SUCCESS, "add edge 2")),
                                createTreeNode(new Step(Step.STATUS_SUCCESS, "close edge 2"), new MutableTreeNode[] { createTreeNode(new Step(Step.STATUS_FAILURE, "apply head_complement rule")),
                                        createTreeNode(new Step(Step.STATUS_FAILURE, "apply head_subject rule")) }) }) }), createTreeNode(new Step(Step.STATUS_SUCCESS, "add edge 3")),
                createTreeNode(new Step(Step.STATUS_PROGRESS, "close edge 3"), new MutableTreeNode[] { createTreeNode(new Step(Step.STATUS_PROGRESS, "apply head_complement rule")) }) });
        JTree result = new JTree(root);
        result.setCellRenderer(new StepRenderer());
        return result;
    }

    private JButton createButton(String filename, String toolTipText)
    {
        JButton result = new JButton(new ImageIcon(IconUtil.getIcon(filename)));
        result.setToolTipText(toolTipText);
        return result;
    }

    private MutableTreeNode createTreeNode(Object userObject)
    {
        return createTreeNode(userObject, new MutableTreeNode[0]);
    }

    private MutableTreeNode createTreeNode(Object userObject, MutableTreeNode[] children)
    {
        DefaultMutableTreeNode result = new DefaultMutableTreeNode(userObject);

        for (MutableTreeNode child : children)
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
        updateChartPanelDisplay();
        updateTreePanelDisplay();
    }
    
    public void updateChartPanelDisplay()
	{	
    	List<Integer> trace = new LinkedList<Integer>();
    	XMLTraceNode node = sld.traceNodes.getData(traceNodeID);
    	while (node.getParent() != null)
    	{
    		trace.add(0,node.id);
       		node = node.getParent();
    	}
    	trace.add(0,node.id);
    	//System.err.println("Trace to determine chart change node: " + trace);
    	sld.curCM = new ChartModel(sld.curCM.words);
    	for (int i : trace)
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
    
    public void updateTreePanelDisplay()
	{		      
        TreeModel dtm = new DecisionTreeModelBuilder().createTreeModel(sld.traceModel);
        System.err.println(dtm.nodes.size());
        TreeView dtv = new TreeView(dtm, 200, 50);
        processColorMarkings(dtv);
        addNodeMarking(dtv,traceNodeID, Color.YELLOW);
        ((TreeViewPanel) dtp).displayTreeView(dtv);
        //viewport change, trying to center decision tree view on active node --> buggy!
        JViewport view = dtvsp.getViewport();
        Point p = new Point(dtv.treeNodes.get(traceNodeID).x - 200,dtv.treeNodes.get(traceNodeID).y - 200);
        view.setViewPosition(p);
	}
    
    private void addNodeMarking(TreeView t, int nodeID, Color color)
    {
    	System.err.println("adding node marking for " + nodeID);
        t.treeNodes.get(nodeID).color = color;
    }
    
    private void processColorMarkings(TreeView t)
    {
        for (int i : nodeColorings.keySet())
        {
            t.treeNodes.get(i).color = nodeColorings.get(i);
        }
    }
}
