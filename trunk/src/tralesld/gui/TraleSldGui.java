package tralesld.gui;

import gralej.parsers.ParseException;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.swing.Box;
import javax.swing.BoxLayout;
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
import javax.swing.JViewport;

import tralesld.TraleSld;
import tralesld.gui.icons.IconUtil;
import tralesld.struct.chart.ChartEdge;
import tralesld.struct.chart.ChartModel;
import tralesld.struct.chart.ChartModelChange;
import tralesld.struct.source.SourceCodeLocation;
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
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	TraleSld sld;
	TraleSldController ctrl;

	VisualizationUtility util;

	JTabbedPane chartPanel;
	JTabbedPane grammarPanel;
	JTabbedPane detailPanel;

	ChartViewPanel cvp;
	// overview tree panel
	public TreeViewPanel otp;
	JScrollPane otvsp;
	// decision tree panel
	public TreeViewPanel dtp;
	JScrollPane dtvsp;

	public SourceCodeViewPanel sourcePanel;
	public JPanel signaturePanel;

	// step detail panel (feature structures etc.)
	JPanel localTreePanel;
	JComponent localTreeTab;
	String currentGraleString;
	public VariableWatchPanel variablesPanel;

	JPanel chartTab;
	JPanel sourceTab;
	JPanel controlFlowTab;

	JButton chartButton;
	JButton sourceButton;
	JButton signatureButton;
	JButton variableButton;
	JButton localTreeButton;
	JButton controlFlowButton;

	public int traceNodeID;
	public int overviewNodeID;
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

		util = VisualizationUtility.getDefault();

		traceNodeID = 0;
		overviewNodeID = 0;
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
		detailPanel.addTab("Local tree", createLocalTreeTab(createLocalTreePanel()));
		localTreeButton = new JButton("Local tree");
		localTreeButton.addMouseListener(ctrl);
		localTreeButton.setBorder(null);
		detailPanel.setTabComponentAt(0, localTreeButton);
		detailPanel.addTab("Variables", createVariablesTab());
		variableButton = new JButton("Variables");
		variableButton.addMouseListener(ctrl);
		variableButton.setBorder(null);
		detailPanel.setTabComponentAt(1, variableButton);
		return detailPanel;
	}

	private JComponent createChartPanel()
	{
		chartPanel = new JTabbedPane();
		chartPanel.setPreferredSize(new Dimension(800, 300));
		chartPanel.addTab("Chart", createChartTab());
		chartPanel.addTab("Control flow graph", createDecisionTreeTab());
		chartButton = new JButton("Chart");
		chartButton.addMouseListener(ctrl);
		chartButton.setBorder(null);
		chartPanel.setTabComponentAt(0, chartButton);
		controlFlowButton = new JButton("Control flow graph");
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
		sourceButton = new JButton("Source");
		sourceButton.addMouseListener(ctrl);
		sourceButton.setBorder(null);
		grammarPanel.setTabComponentAt(0, sourceButton);
		signatureButton = new JButton("Signature");
		signatureButton.addMouseListener(ctrl);
		signatureButton.setBorder(null);
		grammarPanel.setTabComponentAt(1, signatureButton);
		return grammarPanel;
	}

	private JPanel createLocalTreePanel()
	{
		localTreePanel = new JPanel();
		return localTreePanel;
	}

	public JComponent createLocalTreeTab(JComponent localTreePanel)
	{
		JScrollPane scrollPane = new JScrollPane(localTreePanel);
		localTreeTab = new JPanel();
		localTreeTab.setLayout(new BoxLayout(localTreeTab, BoxLayout.Y_AXIS));
		localTreeTab.add(scrollPane);
		return localTreeTab;
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
		controlFlowTab.setBackground(new Color(220, 220, 220));

		otp = new TreeViewPanel();
		otp.setBackground(new Color(220, 220, 220));
		otp.t = new TreeView(null);
		OverviewTreeMouseListener overviewTreeMouseListener = new OverviewTreeMouseListener(otp, this);
		otp.addMouseListener(overviewTreeMouseListener);
		otp.edgyLines = false;

		otvsp = new JScrollPane(otp);
		otvsp.getViewport().setBackground(new Color(220, 220, 220));
		otvsp.setMinimumSize(new Dimension(200, 200));
		otvsp.setPreferredSize(new Dimension(200, 200));
		controlFlowTab.add(otvsp);

		dtp = new TreeViewPanel();
		dtp.setBackground(new Color(220, 220, 220));
		dtp.t = new TreeView(null);
		DecisionTreeMouseListener decTreeMouseListener = new DecisionTreeMouseListener(dtp, this);
		dtp.addMouseListener(decTreeMouseListener);
		dtp.edgyLines = false;

		dtvsp = new JScrollPane(dtp);
		dtvsp.getViewport().setBackground(new Color(220, 220, 220));
		controlFlowTab.add(dtvsp);
		return controlFlowTab;
	}

	private JComponent createControlTab()
	{
		JPanel result = new JPanel();
		result.setLayout(new BoxLayout(result, BoxLayout.Y_AXIS));
		result.add(createControlButtonsPanel());
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

	private JButton createButton(String filename, String toolTipText)
	{
		JButton result = new JButton(new ImageIcon(IconUtil.getIcon(filename)));
		result.setToolTipText(toolTipText);
		return result;
	}

	public static TraleSldGui createAndShowGUI(final TraleSld sld)
	{
		// Create and set up the window.
		JFrame frame = new JFrame("Kahina");
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
		System.err.println("dtncl" + clickedNode);
		selectDecisionTreeNode(clickedNode);
		updateAllDisplays();
	}

	@SuppressWarnings("unchecked")
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
		} else
		{
			dtp.t.getInvisibleNodes().removeAll(descendants);
			dtp.t.getMarkedNodes().remove(clickedNode);
			nodesWithCollapsedDescendants.put(clickedNode, false);
		}
		updateAllDisplays();
	}

	public void overviewTreeNodeClick(int nodeID)
	{
		selectDecisionTreeNode(nodeID);

		// adapt chart view to new selection
		LinkedList<ChartEdge> activeChartEdges = new LinkedList<ChartEdge>();
		ChartEdge rootEdge = sld.edgeRegister.getData(traceNodeID);
		if (rootEdge != null)
			activeChartEdges.add(rootEdge);
		changeActiveChartEdges(activeChartEdges);

		// System.err.println("current decision tree head: " +
		// sld.currentDecisionTreeHead);
		updateAllDisplays();
	}

	public void overviewTreeNodeDblClick(int nodeID)
	{

	}

	public void collapseCallDimension(int nodeID)
	{
		ArrayList<Integer> descendants = (ArrayList<Integer>) sld.getStepChildren(nodeID).clone();
		for (int i = 0; i < descendants.size(); i++)
		{
			if (!dtp.t.getMarkedNodes().contains(descendants.get(i)))
			{
				descendants.addAll(sld.getStepChildren(descendants.get(i)));
			}
		}
		System.err.println("\tDescendants: " + descendants);
		dtp.t.getInvisibleNodes().addAll(descendants);
		dtp.t.getMarkedNodes().add(nodeID);
		nodesWithCollapsedDescendants.put(nodeID, true);
	}

	public void decollapseCallDimUntilNodeVisible(int nodeID)
	{
		if (dtp.t.getInvisibleNodes().contains(nodeID))
		{
			dtp.t.getInvisibleNodes().remove(nodeID);
			int parent = sld.stepAncestors.getData(nodeID);
			while (dtp.t.getInvisibleNodes().contains(parent))
			{
				dtp.t.getInvisibleNodes().remove(parent);
				parent = sld.stepAncestors.getData(parent);
			}
		}
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

	public void updateLocalTreeDisplay()
	{
		Map<String, String> data = sld.nodeData.getData(traceNodeID);

		if (data != null && data.containsKey(TraleSld.LOCAL_TREE_KEY))
		{
			String featDetail = data.get(TraleSld.LOCAL_TREE_KEY);

			if (!featDetail.equals(currentGraleString))
			{
				localTreePanel.removeAll();
				try
				{
					JPanel detail = util.visualize(featDetail);
					localTreePanel.add(detail);
					localTreePanel.add(detail);
					currentGraleString = featDetail;
				} catch (ParseException e)
				{
					JPanel errorPanel = new JPanel();
					errorPanel.add(new JLabel("Parse error: " + e.getMessage()));
					localTreePanel.add(errorPanel);
					currentGraleString = null;
				}
				this.repaint();
			}
		} else
		{
			localTreePanel.removeAll();
			localTreePanel.add(new JLabel("no local tree at this step"));
			this.repaint();
			currentGraleString = null;
		}
	}

	public void updateVariableDisplay()
	{
		System.out.println("UVD");
		Map<String, String> data = sld.nodeData.getData(traceNodeID);
		if (data == null)
		{
			System.out.println("A2UVP");
			variablesPanel.update(data);
		} else
		{
			// HACK: create new map that doesn't contain the local tree
			Map<String, String> newData = new HashMap<String, String>(data.size());
			for (String key : data.keySet())
			{
				if (!TraleSld.LOCAL_TREE_KEY.equals(key))
				{
					newData.put(key, data.get(key));
				}
			}
			variablesPanel.update(newData);
		}
	}

	public void updateOverviewTreePanelDisplay()
	{
		TreeView otv = new OverviewTreeViewBuilder().createOverviewTreeView(sld);
		otv.nodeShape = TreeView.BOX_SHAPE;
		processColorMarkings(otv);
		otv.calculateCoordinates();
		otv.setSelectedNode(overviewNodeID);
		((TreeViewPanel) otp).displayTreeView(otv);
		// hand on selection information via a non-standard means
		if (otv.treeNodes.get(overviewNodeID) != null)
		{
			otv.treeNodes.get(overviewNodeID).setEdgeDir("sel");
		}
	}

	public void updateDecisionTreePanelDisplay()
	{
		TreeView dtv = new DecisionTreeViewBuilder().createDecisionTreeView(sld);
		dtv.nodeShape = TreeView.BOX_SHAPE;
		processColorMarkings(dtv);
		dtv.setInvisibleNodes(dtp.t.getInvisibleNodes());
		dtv.setMarkedNodes(dtp.t.getMarkedNodes());
		dtv.setSelectedNode(traceNodeID);
		dtv.calculateCoordinates();
		((TreeViewPanel) dtp).displayTreeView(dtv);
		// hand on selection information via a non-standard means
		if (dtv.treeNodes.get(traceNodeID) != null)
		{
			dtv.treeNodes.get(traceNodeID).setEdgeDir("sel");
		}
	}

	public void centerViewsOnCurrentNode()
	{
		if (traceNodeID != -1)
		{
			decollapseCallDimUntilNodeVisible(traceNodeID);
			// viewport change, trying to center decision tree view on active
			// node
			JViewport view = dtvsp.getViewport();
			TreeViewNode n = ((TreeViewPanel) dtp).t.treeNodes.get(traceNodeID);
			Point p = new Point(n.x - dtvsp.getWidth() / 2, n.y - dtvsp.getHeight() / 2);
			view.setViewPosition(p);
		}
		if (overviewNodeID != -1)
		{
			// viewport change, trying to center overview tree view on active
			// node
			JViewport view = otvsp.getViewport();
			TreeViewNode n = ((TreeViewPanel) otp).t.treeNodes.get(overviewNodeID);
			Point p = new Point(n.x - otvsp.getWidth() / 2, n.y - otvsp.getHeight() / 2);
			view.setViewPosition(p);
		}
	}

	public void updateSourceDisplay()
	{
		SourceCodeLocation loc = sld.sourceLocations.getData(traceNodeID);
		if (loc != null)
		{
			sourcePanel.displaySourceCodeLocation(loc);
		} else
		{
			System.err.println("Source location not found!");
		}
	}

	public void updateAllDisplays()
	{
		System.out.println("UAD");
		updateChartPanelDisplay();
		updateSourceDisplay();
		updateLocalTreeDisplay();
		updateVariableDisplay();
		updateOverviewTreePanelDisplay();
		updateDecisionTreePanelDisplay();
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

	public void selectDecisionTreeNode(int node)
	{
		traceNodeID = node;
		overviewNodeID = sld.tracer.getOverviewAncestor(node);
		sld.currentDecisionTreeHead = overviewNodeID;
		selectChartEdge(sld.nodeToEdge.getData(node));
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
		} else
		{
			// System.out.println("Trying to select null edge!");
		}
	}
}
