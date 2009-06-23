package tralesld.gui;

import gralej.parsers.ParseException;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTree;
import javax.swing.JViewport;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreePath;

import tralesld.TraleSld;
import tralesld.gui.icons.IconUtil;
import tralesld.storage.DataStore;
import tralesld.struct.chart.ChartEdge;
import tralesld.struct.chart.ChartModel;
import tralesld.struct.chart.ChartModelChange;
import tralesld.struct.source.SourceCodeLocation;
import tralesld.struct.tree.DecisionTreeModelBuilder;
import tralesld.struct.tree.TreeModelNode;
import tralesld.util.VisualizationUtility;
import tralesld.visual.bindings.VariableWatchPanel;
import tralesld.visual.chart.ChartView;
import tralesld.visual.chart.ChartViewBuilder;
import tralesld.visual.chart.ChartViewMouseListener;
import tralesld.visual.chart.ChartViewPanel;
import tralesld.visual.source.SourceCodeViewPanel;
import tralesld.visual.tree.TreeView;
import tralesld.visual.tree.TreeViewNode;
import tralesld.visual.tree.TreeViewPanel;

public class TraleSldGui extends JPanel
{
    TraleSld sld;
    TraleSldController ctrl;

    VisualizationUtility util;

    JTabbedPane chartPanel;
    JTabbedPane grammarPanel;
    JTabbedPane detailPanel;

    ChartViewPanel cvp;
    // decision tree panel
    public TreeViewPanel dtp;
    JScrollPane dtvsp;

    public SourceCodeViewPanel sourcePanel;
    public JPanel signaturePanel;
    public VariableWatchPanel variablesPanel;

    // step detail panel (feature structures etc.)
    JPanel stepDetailPanel;
    String currentGraleString;

    JPanel chartTab;
    JPanel sourceTab;
    JPanel controlFlowTab;

    JButton chartButton;
    JButton sourceButton;
    JButton signatureButton;
    JButton variableButton;
    JButton stepDetailButton;
    JButton controlFlowButton;

    public JTree overviewTree;
    DefaultTreeModel overviewTreeModel;
    DefaultMutableTreeNode overviewTreeRoot;

    // map chart edges via their IDs to associated overview tree nodes
    public DataStore<DefaultMutableTreeNode> stepRegister;

    public int traceNodeID;
    public HashMap<Integer, Boolean> nodesWithCollapsedDescendants;
    public HashMap<Integer, Color> nodeColorings;

    // register active chart edges for proper redrawing
    public List<ChartEdge> activeChartEdges;

    public TraleSldGui(TraleSldController ctrl)
    {
        super(new GridLayout(1, 0));
        this.ctrl = ctrl;
        add(createVerticalSplit());
        ctrl.setGUI(this);

        util = new VisualizationUtility();

        stepRegister = new DataStore<DefaultMutableTreeNode>();

        traceNodeID = 0;
        nodesWithCollapsedDescendants = new HashMap<Integer, Boolean>();
        nodeColorings = new HashMap<Integer, Color>();
        activeChartEdges = new LinkedList<ChartEdge>();
    }

    private JComponent createVerticalSplit()
    {
        JSplitPane result = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, createLeftPanel(), createRightPanel());
        result.setResizeWeight(0);
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
        result.setResizeWeight(0.5);
        return result;
    }

    private JComponent createStepDetailPanel()
    {
        detailPanel = new JTabbedPane();
        detailPanel.addTab("Detail", createStepDetailTab());
        stepDetailButton = new JButton("Detail");
        stepDetailButton.addMouseListener(ctrl);
        stepDetailButton.setBorder(null);
        detailPanel.setTabComponentAt(0, stepDetailButton);
        return detailPanel;
    }

    private JComponent createChartPanel()
    {
        chartPanel = new JTabbedPane();
        chartPanel.setPreferredSize(new Dimension(800, 300));
        chartPanel.addTab("Chart", createChartTab());
        chartPanel.addTab("Control Flow Graph", createDecisionTreeTab());
        chartButton = new JButton("Chart");
        chartButton.addMouseListener(ctrl);
        chartButton.setBorder(null);
        chartPanel.setTabComponentAt(0, chartButton);
        controlFlowButton = new JButton("Control Flow Graph");
        controlFlowButton.addMouseListener(ctrl);
        controlFlowButton.setBorder(null);
        chartPanel.setTabComponentAt(1, controlFlowButton);

        return chartPanel;
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
        grammarPanel = new JTabbedPane();
        grammarPanel.addTab("Source", createSourceTab());
        grammarPanel.addTab("Signature", createSignatureTab());
        grammarPanel.addTab("Variables", createVariablesTab());
        sourceButton = new JButton("Source");
        sourceButton.addMouseListener(ctrl);
        sourceButton.setBorder(null);
        grammarPanel.setTabComponentAt(0, sourceButton);
        signatureButton = new JButton("Signature");
        signatureButton.addMouseListener(ctrl);
        signatureButton.setBorder(null);
        grammarPanel.setTabComponentAt(1, signatureButton);
        variableButton = new JButton("Variables");
        variableButton.addMouseListener(ctrl);
        variableButton.setBorder(null);
        grammarPanel.setTabComponentAt(2, variableButton);
        return grammarPanel;
    }

    private JComponent createStepDetailTab()
    {
        stepDetailPanel = new JPanel();
        JScrollPane scrollPane = new JScrollPane(stepDetailPanel);
        JPanel detailTab = new JPanel();
        detailTab.setLayout(new BoxLayout(detailTab, BoxLayout.Y_AXIS));
        detailTab.add(scrollPane);
        return detailTab;
    }

    private JComponent createChartTab()
    {
        chartTab = new JPanel();
        chartTab.setLayout(new BoxLayout(chartTab, BoxLayout.Y_AXIS));
        chartTab.add(createChartControlPanel());

        cvp = new ChartViewPanel();
        ChartViewMouseListener traleChartMouseListener = new TraleChartMouseListener(cvp, this);
        cvp.addMouseListener(traleChartMouseListener);
        cvp.v = new ChartView();

        JScrollPane scrollPane = new JScrollPane(cvp);
        scrollPane.setBackground(Color.WHITE);
        chartTab.add(scrollPane);
        chartTab.addMouseListener(ctrl);
        return chartTab;
    }

    private JComponent createDecisionTreeTab()
    {
        controlFlowTab = new JPanel();
        controlFlowTab.setLayout(new BoxLayout(controlFlowTab, BoxLayout.Y_AXIS));
        controlFlowTab.add(createTreeControlPanel());

        dtp = new TreeViewPanel();
        dtp.t = new TreeView(null);
        DecisionTreeMouseListener decTreeMouseListener = new DecisionTreeMouseListener(dtp, this);
        dtp.addMouseListener(decTreeMouseListener);
        dtp.edgyLines = false;

        dtvsp = new JScrollPane(dtp);
        dtvsp.setBackground(Color.WHITE);
        controlFlowTab.add(dtvsp);
        return controlFlowTab;
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
        signaturePanel = new JPanel();
        return signaturePanel;
    }

    private JComponent createVariablesTab()
    {
        variablesPanel = new VariableWatchPanel();
        return variablesPanel;
    }

    private JComponent createSourceTab()
    {
        sourcePanel = new SourceCodeViewPanel();   
        return sourcePanel;
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
        JButton skipButton = createButton("roundskip.png", "auto-complete this step");
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

    public static TraleSldGui createAndShowGUI(final TraleSld sld)
    {
        // Create and set up the window.
        JFrame frame = new JFrame("TraleSLD");
        frame.addWindowListener(new WindowAdapter()
        {

            @Override
            public void windowClosing(WindowEvent e)
            {
                sld.stop();
            }

        });

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

    public void decisionTreeNodeDblClick(int clickedNode)
    {
        // System.err.println("Registered double click on node " + clickedNode);
        // toggle visibility of all the tree nodes spawned by the clicked node
        ArrayList<Integer> descendants = (ArrayList<Integer>) sld.getStepChildren(clickedNode).clone();
        for (int i = 0; i < descendants.size(); i++)
        {
            if (!dtp.t.getMarkedNodes().contains(descendants.get(i)))
            {
                descendants.addAll(sld.getStepChildren(descendants.get(i)));
            }
        }
        System.err.println("\tDescendants: " + descendants);
        if (nodesWithCollapsedDescendants.get(clickedNode) == null || !nodesWithCollapsedDescendants.get(clickedNode))
        {
            dtp.t.getInvisibleNodes().addAll(descendants);
            dtp.t.getMarkedNodes().add(clickedNode);
            nodesWithCollapsedDescendants.put(clickedNode, true);
        }
        else
        {
            dtp.t.getInvisibleNodes().removeAll(descendants);
            dtp.t.getMarkedNodes().remove(clickedNode);
            nodesWithCollapsedDescendants.put(clickedNode, false);
        }
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
        if (sld.nodeData.getData(traceNodeID) != null)
        {
            String featDetail = sld.nodeData.getData(traceNodeID).get(TraleSld.STEPDETAIL_DEFAULT_KEY); // default                                                                                                 // key
            if (!featDetail.equals(currentGraleString))
            {
                stepDetailPanel.removeAll();
                try
                {
                    JPanel detail = util.visualize(featDetail);
                    stepDetailPanel.add(detail);
                    stepDetailPanel.add(detail);
                    currentGraleString = featDetail;
                }
                catch (ParseException e)
                {
                    JPanel errorPanel = new JPanel();
                    errorPanel.add(new JLabel("Parse error: " + e.getMessage()));
                    stepDetailPanel.add(errorPanel);
                    currentGraleString = null;
                }
                this.repaint();
            }
        }
        else
        {
            stepDetailPanel.removeAll();
            stepDetailPanel.add(new JLabel("No step detail found"));
            this.repaint();
            currentGraleString = null;
        }
    }

    public void updateTreeOverview()
    {
        // save selected path to be restored later
        TreePath selectionPath = overviewTree.getSelectionPath();
        // System.err.println("Rebuild with following selection path: \n" +
        // selectionPathToString(selectionPath));

        // extend overview tree with information from new nodes
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
        dtv.setInvisibleNodes(dtp.t.getInvisibleNodes());
        dtv.setMarkedNodes(dtp.t.getMarkedNodes());
        dtv.calculateCoordinates();
        ((TreeViewPanel) dtp).displayTreeView(dtv);
        // hand on selection information via a non-standard means
        if (dtv.treeNodes.get(traceNodeID) != null)
        {
            dtv.treeNodes.get(traceNodeID).setEdgeDir("sel");
            // viewport change, trying to center decision tree view on active
            // node --> buggy!
            JViewport view = dtvsp.getViewport();
            Point p = new Point(dtv.treeNodes.get(traceNodeID).x - 200, dtv.treeNodes.get(traceNodeID).y - 200);
            view.setViewPosition(p);
        }
    }

    public void updateSourceDisplay()
    {
        SourceCodeLocation loc = sld.sourceLocations.getData(traceNodeID);
        if (loc != null)
        {
            sourcePanel.displaySourceCodeLocation(loc);
        }
        else
        {
            System.err.println("Source location not found!");
        }
    }

    public void updateAllDisplays()
    {
        updateChartPanelDisplay();
        updateSourceDisplay();
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
        for (TreeViewNode n : t.treeNodes.values())
        {
            n.color = nodeColorings.get(n.id);
            if (n.color == null)
            {
                n.color = Color.white;
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
            List<Integer> daughters = sld.chartDependencies.getData(e.id);
            if (daughters != null)
            {
                for (int daughterID : daughters)
                {
                    elist.add(sld.curCM.edges.get(daughterID));
                }
            }
            changeActiveChartEdges(elist);

            TreePath oldSelectionPath = overviewTree.getSelectionPath();
            // System.err.println("Old " +
            // selectionPathToString(oldSelectionPath));
            TreePath selectionPath = new TreePath(stepRegister.getData(e.id).getPath());
            // System.err.println("New " +
            // selectionPathToString(selectionPath));
            if (oldSelectionPath == null || !selectionPath.isDescendant(oldSelectionPath))
            {
                // System.err.println("Changing selection path!");
                ctrl.ignoreNextOverviewTreeSelectionChange();
                overviewTree.scrollPathToVisible(selectionPath);
                overviewTree.setSelectionPath(selectionPath);
            }
        }
        else
        {
            // System.out.println("Trying to select null edge!");
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
        if (selectionPath == null)
            return "null";
        String result = "selection path:\n";
        for (Object n : selectionPath.getPath())
        {
            Step step = (Step) ((DefaultMutableTreeNode) n).getUserObject();
            result += step.toString() + "\n";
        }
        return result;
    }
}
