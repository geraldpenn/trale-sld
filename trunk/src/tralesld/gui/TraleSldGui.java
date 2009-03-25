package tralesld.gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Point;
import java.util.ArrayList;
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
    
    int traceNodeID;

    public TraleSldGui(TraleSldController ctrl)
    {
        super(new GridLayout(1, 0));
        add(createVerticalSplit());
        ctrl.gui = this;
        this.ctrl = ctrl;
        traceNodeID = -1;
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

        JScrollPane scrollPane = new JScrollPane(dtp);
        scrollPane.setBackground(Color.WHITE);
        result.add(scrollPane);
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
        result.add(createButton("creep.png", "continue"));
        result.add(Box.createHorizontalGlue());
        result.add(createButton("skip.png", "auto-complete this step"));
        result.add(Box.createHorizontalGlue());
        result.add(createButton("reject.png", "make this step fail"));
        result.add(Box.createHorizontalGlue());
        result.add(createButton("leap.png", "auto-complete"));
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
        // demo code to mimic mockup behaviour
        ArrayList<String> wordList = new ArrayList<String>();
        wordList.add("it");
        wordList.add("walks");
        ChartModel cm = new ChartModel(wordList);
        cm.edges.add(new ChartEdge(1, 2, "0 lexicon", 1, true));
        cm.edges.add(new ChartEdge(0, 1, "1 lexicon", 1, false));
        cm.edges.add(new ChartEdge(1, 2, "head_complement", 0, false));
        cm.edges.add(new ChartEdge(1, 2, "head_subject", 0, false));
        cm.edges.add(new ChartEdge(0, 2, "head_complement", 0, false));
        cm.edges.add(new ChartEdge(0, 2, "2 head_subject", 1, false));
        cm.edges.add(new ChartEdge(0, 1, "3 lexicon", 1, true));
        cm.edges.add(new ChartEdge(0, 2, "head_complement", 0, false));
        cm.edges.add(new ChartEdge(0, 2, "head_subject", 2, true));
        
        gui.cvp.v = ChartViewBuilder.buildChartView(cm, true);
        
        sld.curCM = cm;
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
    }
    
    public void updateChartPanelDisplay()
	{		
    	List<Integer> trace = new LinkedList<Integer>();
    	XMLTraceNode node = sld.traceNodes.getData(traceNodeID);
    	while (node.getParent() != null)
    	{
    		trace.add(trace.size(),node.id);
    	}
    	sld.curCM = new ChartModel(sld.curCM.words);
    	for (int i : trace)
    	{
    		sld.curCM.processChange(sld.chartChanges.getData(i));
    	}
    	cvp.v = ChartViewBuilder.buildChartView(sld.curCM, true);
	}
    
    public void updateTreePanelDisplay()
	{		      
        TreeModel dtm = new DecisionTreeModelBuilder().createTreeModel(sld.traceModel);
        TreeView dtv = new TreeView(dtm, 200, 50);
        ((TreeViewPanel) dtp).displayTreeView(dtv);
	}

    /**
     * @param args
     */
    public static void main(String[] args)
    {
        javax.swing.SwingUtilities.invokeLater(new Runnable()
        {
            public void run()
            {
                TraleSld sld = new TraleSld();
            }
        });
    }
}
