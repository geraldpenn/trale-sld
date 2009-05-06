package tralesld;

import java.awt.Color;
import java.io.IOException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.*;

import tralesld.gui.*;
import tralesld.storage.*;
import tralesld.struct.chart.*;
import tralesld.struct.source.*;
import tralesld.struct.trace.*;
import tralesld.struct.tree.*;
import tralesld.util.*;
import tralesld.visual.tree.TreeViewPanel;

public class TraleSld
{
	TraleSldGui gui;

	// database connection
	private Connection connection;

	// current chart model
	public ChartModel curCM;

	// chart is stored in form of chart changes for each trace node
	public DataStore<List<ChartModelChange>> chartChanges;
	public DataStore<List<Integer>> chartDependencies;
	// index chart edges by TRALE numbering
	public DataStore<ChartEdge> chartEdges;

	// encode tree structure in first dimension: decision tree
	public DataStore<XMLTraceNode> traceNodes;

	// encode tree structure in second dimension: call tree
	public DataStore<Integer> stepAncestors;
	public DataStore<Integer> recursionDepths;

	public DataStore<Integer> stepFollowers;
	public DataStore<Integer> stepStatus;
	public DataStore<String> nodeCommands;

	// contains source code location for each step
	public DataStore<SourceCodeLocation> sourceLocations;

	// store feature structure representations, bindings etc. here
	public DataStore<Map<String, String>> nodeData;

	// map overview tree nodes via stepIDs to associated edges
	public DataStore<ChartEdge> edgeRegister;

	// build feature structure string currently transmitted in this buffer
	public StringBuilder builder;

	public Tracer tracer;

	public XMLTraceNode currentDecisionTreeHead;

	public int currentDecisionTreeNode = 0;

	public int lastActiveID = -1;

	ChartEdge lastEdge;

	ChartModelChange currentLexicalEdge;

	TreeModelNode currentOverviewTreeNode;

	LinkedList<ChartEdge> activeEdgeStack;

	HashSet<ChartEdge> successfulEdges;

	// current signal to prolog
	public char reply = 'n';

	boolean autoCompleteMode;

	// during skip node, the step ID of the skipped step is stored here
	public int skipToStep;

	boolean inSkip;

	public void start()
	{
		System.err.print("Trying to build GUI window... ");
		try
		{
			startDatabase();
			gui = TraleSldGui.createAndShowGUI(this);
			autoCompleteMode = false;
			skipToStep = -1;
			inSkip = false;
			System.err.println("Success.");
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	private void startDatabase() throws ClassNotFoundException, SQLException, IOException
	{
		Class.forName("org.apache.derby.jdbc.EmbeddedDriver");
		connection = DriverManager.getConnection("jdbc:derby:traleslddb.tmp;create=true");
		Statement statement = connection.createStatement();

		try
		{
			statement.executeUpdate("DROP TABLE data");
		}
		catch (SQLException e)
		{
			// ignore - gotta hate Derby for not supporting DROP TABLE IF EXISTS
		}

		statement.executeUpdate("CREATE TABLE data (id BIGINT NOT NULL , value VARCHAR(32) NOT NULL, PRIMARY KEY (id))");
		statement.close();
	}

	public void stop()
	{
		try
		{
			connection.close();
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}

		// TODO tell TRALE to abort parsing process, don't exit
		System.exit(0);
	}

	public void initializeParseTrace(String parsedSentenceList)
	{
		System.err.println("Trying to initialize parse trace... ");
		try
		{
			List<String> wordList = PrologUtilities.parsePrologStringList(parsedSentenceList);
			curCM = new ChartModel(wordList);
			chartChanges = new DataStore<List<ChartModelChange>>();
			chartDependencies = new DataStore<List<Integer>>();
			chartEdges = new DataStore<ChartEdge>();
			traceNodes = new DataStore<XMLTraceNode>();
			stepAncestors = new DataStore<Integer>();
			recursionDepths = new DataStore<Integer>();
			recursionDepths.put(0, 0);
			stepFollowers = new DataStore<Integer>();
			stepStatus = new DataStore<Integer>();
			List<Integer> nodeToMark = new ArrayList<Integer>();
			gui.dtp.viewExtensionsBeforeMainRendering.add(new CallDimensionViewExtension(stepAncestors, recursionDepths, nodeToMark));
			// gui.dtp.viewExtensionsBeforeMainRendering.add(new
			// ReturnDimensionViewExtension(stepFollowers, gui.nodeColorings));
			gui.dtp.viewExtensionsAfterMainRendering.add(new NodeMarkingViewExtension(nodeToMark, Color.YELLOW));
			gui.dtp.setVisibleEdges(false);
			gui.dtp.setNodePositioning(TreeViewPanel.LEFT_ALIGNMENT);
			// gui.dtp.viewExtensionsAfterMainRendering.add(new
			// SelectionAreaExtension());

			sourceLocations = new DataStore<SourceCodeLocation>();
			nodeCommands = new DataStore<String>();
			nodeCommands.put(0, "init");
			nodeData = new DataStore<Map<String, String>>();
			edgeRegister = new DataStore<ChartEdge>();

			builder = new StringBuilder();

			activeEdgeStack = new LinkedList<ChartEdge>();
			successfulEdges = new HashSet<ChartEdge>();

			tracer = new Tracer();
			traceNodes.put(0, tracer.detailedTraceModel.root);
			tracer.overviewTraceModel.addNode(new TreeModelNode(0, "parsing " + wordList));
			tracer.overviewTraceModel.root = 0;
			currentDecisionTreeHead = tracer.detailedTraceModel.root;

			currentLexicalEdge = null;

			currentOverviewTreeNode = tracer.overviewTraceModel.nodes.get(0);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public void registerStepInformation(int id, String command)
	{
		System.err.println("Trying to register step information (" + id + "," + command + ")... ");
		try
		{
			nodeCommands.put(id, command);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public void registerStepSourceCodeLocation(int id, String absolutePath, int lineNumber)
	{
		System.err.println("Trying to register source code location (" + id + "," + absolutePath + "," + lineNumber + ")... ");
		sourceLocations.put(id, new SourceCodeLocation(absolutePath, lineNumber - 1));
		gui.updateSourceDisplay();
	}

	public void registerRuleApplication(int id, int left, int right, String ruleName)
	{
		System.err.println("Trying to register rule application (" + id + "," + ruleName + "," + left + "," + right + ")... ");
		try
		{
			nodeCommands.put(id, "rule(" + ruleName + ")");
			ChartEdge currentEdge = new ChartEdge(left, right, ruleName, ChartEdge.ACTIVE, true);
			ChartModelChange cmc = new ChartModelChange(1, currentEdge);
			addChartChange(id, cmc);
			activeEdgeStack.add(0, currentEdge);

			TreeModelNode newNode = new TreeModelNode(id, ruleName);
			tracer.overviewTraceModel.addNode(newNode);
			currentOverviewTreeNode.children.add(id);
			newNode.parent = currentOverviewTreeNode.id;
			currentOverviewTreeNode = newNode;
			stepStatus.put(id, Step.STATUS_PROGRESS);
			edgeRegister.put(id, currentEdge);
			lastEdge = currentEdge;

			if (skipToStep == -1)
			{
				gui.updateAllDisplays();
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public void registerStepLocation(String callStack)
	{
		System.err.println("Trying to register step location (" + callStack + ")... ");
		try
		{
			List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
			int stepID = stack.get(0);
			stepFollowers.put(lastActiveID, stepID);
			lastActiveID = stepID;
			int ancestorID = stack.get(1);
			stepAncestors.put(stepID, ancestorID);
			recursionDepths.put(stepID, stack.size() - 1);
			XMLTraceNode newNode = tracer.registerStepAsChildOf(currentDecisionTreeNode, stepID, stepID, nodeCommands.getData(stepID));
			traceNodes.put(stepID, newNode);
			stepFollowers.put(lastActiveID, stepID);
			currentDecisionTreeNode = stepID;
			gui.traceNodeID = stepID;
			if (nodeCommands.getData(stepID).startsWith("rule_close"))
			{
				if (currentLexicalEdge != null)
				{
					addChartChange(stepID, currentLexicalEdge);
					currentLexicalEdge = null;
				}
				TreeModelNode newOverviewNode = new TreeModelNode(stepID, lastEdge.desc);
				tracer.overviewTraceModel.addNode(newOverviewNode);
				currentOverviewTreeNode.children.add(stepID);
				newOverviewNode.parent = currentOverviewTreeNode.id;
				currentOverviewTreeNode = newOverviewNode;
				edgeRegister.put(stepID, lastEdge);
				stepStatus.put(stepID, Step.STATUS_PROGRESS);
				if (skipToStep == -1)
				{
					gui.updateAllDisplays();
					gui.selectChartEdge(lastEdge);
				}
			}
			else if (nodeCommands.getData(stepID).startsWith("rule"))
			{
				if (skipToStep == -1)
				{
					gui.updateAllDisplays();
					gui.selectChartEdge(lastEdge);
				}
			}
			else
			{
				if (skipToStep == -1)
				{
					gui.updateAllDisplays();
				}
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public void registerStepRedo(String callStack)
	{
		System.err.println("Trying to register step redo (" + callStack + ")... ");
		try
		{
			List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
			int stepID = stack.remove(0);
			stepFollowers.put(lastActiveID, stepID);
			lastActiveID = stepID;
			gui.nodeColorings.put(stepID, Color.ORANGE);
			currentDecisionTreeNode = stepID;
			gui.traceNodeID = currentDecisionTreeNode;
			if (skipToStep == -1)
			{
				gui.selectChartEdge(lastEdge);
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public void registerStepExit(String callStack)
	{
		System.err.println("Trying to register step exit (" + callStack + ")... ");
		try
		{
			List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
			int stepID = stack.remove(0);
			gui.nodeColorings.put(stepID, Color.GREEN);
			stepFollowers.put(lastActiveID, stepID);
			lastActiveID = stepID;
			gui.traceNodeID = stepID;
			if (stepID == skipToStep)
			{
				inSkip = false;
				skipToStep = -1;
				reply = 'n';
			}
			if (skipToStep == -1)
			{
				gui.updateAllDisplays();
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public void registerStepFinished(String callStack)
	{
		System.err.println("Trying to register step finished (" + callStack + ")... ");
		try
		{
			List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
			int stepID = stack.remove(0);
			stepFollowers.put(lastActiveID, stepID);
			lastActiveID = stepID;
			gui.nodeColorings.put(stepID, Color.CYAN);
			currentDecisionTreeNode = stack.remove(0);
			gui.traceNodeID = currentDecisionTreeNode;
			if (nodeCommands.getData(stepID).startsWith("rule_close"))
			{
				stepStatus.put(stepID, Step.STATUS_SUCCESS);
				// move up one level in overview tree
				currentOverviewTreeNode = tracer.overviewTraceModel.nodes.get(currentOverviewTreeNode.parent);
				lastEdge = edgeRegister.getData(currentOverviewTreeNode.id);
			}
			if (skipToStep == -1)
			{
				gui.selectChartEdge(lastEdge);
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public void registerStepFailure(String callStack)
	{
		System.err.println("Trying to register step failure (" + callStack + ")... ");
		try
		{
			List<Integer> stack = PrologUtilities.parsePrologIntegerList(callStack);
			int stepID = stack.remove(0);
			stepFollowers.put(lastActiveID, stepID);
			lastActiveID = stepID;
			String command = nodeCommands.getData(stepID);
			// need to handle bug: step failure is called even if edge was
			// successful
			if (command.startsWith("rule("))
			{
				ChartEdge currentEdge = activeEdgeStack.remove(0);
				if (successfulEdges.contains(currentEdge))
				{
					System.err.println("Successful edge! Deleting from chart model...");
					ChartModelChange toDelete = null;
					for (ChartModelChange cmc : chartChanges.getData(stepID))
					{
						if (cmc.edge == currentEdge)
						{
							toDelete = cmc;
							break;
						}
					}
					chartChanges.getData(stepID).remove(toDelete);
					stepStatus.put(stepID, Step.STATUS_SUCCESS);
					gui.nodeColorings.put(stepID, Color.GREEN);
				}
				// current rule application failed; adapt chart accordingly
				else
				{
					System.err.println("Failed edge! Leaving it on the chart as junk...");
					currentEdge.status = ChartEdge.FAILED;
					currentEdge.active = false;
					stepStatus.put(stepID, Step.STATUS_FAILURE);
					gui.nodeColorings.put(stepID, Color.RED);
				}
				// move up one level in overview tree
				currentOverviewTreeNode = tracer.overviewTraceModel.nodes.get(currentOverviewTreeNode.parent);
				lastEdge = edgeRegister.getData(currentOverviewTreeNode.id);
			}
			else
			{
				gui.nodeColorings.put(stepID, Color.RED);
			}
			currentDecisionTreeNode = stack.remove(0);
			gui.traceNodeID = currentDecisionTreeNode;
			if (stepID == skipToStep)
			{
				inSkip = false;
				skipToStep = -1;
				reply = 'n';
			}
			if (skipToStep == -1)
			{
				gui.selectChartEdge(lastEdge);
				gui.updateAllDisplays();
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public void registerChartEdge(int number, int left, int right, String ruleName)
	{
		System.err.println("Trying to register chartEdge (" + number + "," + left + "," + right + "," + ruleName + ")... ");
		try
		{
			lastEdge = new ChartEdge(left, right, number + " " + ruleName, ChartEdge.SUCCESSFUL, true);
			chartEdges.put(number, lastEdge);
			ChartModelChange cmc = new ChartModelChange(1, lastEdge);
			if (ruleName.equals("lexicon"))
			{
				currentLexicalEdge = cmc;
			}
			else
			{
				int dtNode = findRuleAncestor(currentDecisionTreeNode);
				addChartChange(dtNode, cmc);
				if (activeEdgeStack.size() > 0)
				{
					System.err.println("Marking the following edge as successful: " + activeEdgeStack.get(0));
					successfulEdges.add(activeEdgeStack.get(0));
				}
				currentDecisionTreeHead = traceNodes.getData(dtNode);
				gui.traceNodeID = dtNode;
				if (skipToStep == -1)
				{
					gui.updateAllDisplays();
				}
			}
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public void registerEdgeDependency(int motherID, int daughterID)
	{
		try
		{
			int internalMotherID = chartEdges.getData(motherID).id;
			if (chartDependencies.getData(internalMotherID) == null)
			{
				chartDependencies.put(internalMotherID, new ArrayList<Integer>());
			}
			chartDependencies.getData(internalMotherID).add(chartEdges.getData(daughterID).id);
		}
		catch (Exception e)
		{
			e.printStackTrace();
		}
	}

	public void registerMessageChunk(int stepID, String chunk)
	{
		builder.append(chunk);
	}

	public void registerMessageEnd(int stepID)
	{
		registerMessageEnd(stepID, "call");
	}

	public void registerMessageEnd(int stepID, String type)
	{
		if (nodeData.getData(stepID) == null)
		{
			nodeData.put(stepID, new HashMap<String, String>());
		}
		nodeData.getData(stepID).put(type, builder.toString());
		builder = new StringBuilder();
	}

	private int findRuleAncestor(int dtNode)
	{
		XMLTraceNode node = traceNodes.getData(dtNode);
		while (!nodeCommands.getData(node.id).startsWith("rule(") && !nodeCommands.getData(node.id).equals("init"))
		{
			node = node.getParent();
		}
		return node.id;
	}

	public char getPressedButton()
	{
		char oldReply = reply;
		if (reply == 'l')
			return 'c';
		if (reply == 's')
		{
			if (currentDecisionTreeNode == skipToStep)
			{
				if (!inSkip)
				{
					inSkip = true;
					return 'c';
				}
				else
				{
					inSkip = false;
					skipToStep = -1;
					reply = 'n';
					return 'n';
				}
			}
			else
			{
				return 'c';
			}
		}
		reply = 'n';
		return oldReply;
	}

	public void addChartChange(int nodeID, ChartModelChange cmc)
	{
		if (chartChanges.getData(nodeID) == null)
		{
			chartChanges.put(nodeID, new ArrayList<ChartModelChange>());
		}
		chartChanges.getData(nodeID).add(cmc);
	}

	public static void main(String[] args) throws InterruptedException
	{
		TraleSld sld = new TraleSld();
		sld.start();
		sld.initializeParseTrace("[it,walks]");
		Thread.sleep(500);
		sld.registerChartEdge(0, 1, 2, "lexicon");
		Thread.sleep(500);
		sld.registerStepInformation(1, "rule_close");
		sld.registerStepLocation("[1,0]");
		sld.registerStepSourceCodeLocation(1, "/home/johannes/.bashrc", 1);
		Thread.sleep(500);
		sld.registerRuleApplication(2, 1, 3, "head_subject");
		sld.registerStepLocation("[2,1,0]");
		sld.registerStepSourceCodeLocation(2, "/home/johannes/.bashrc", 2);
		Thread.sleep(500);
		sld.registerStepInformation(3, "unify");
		sld.registerStepLocation("[3,2,1,0]");
		Thread.sleep(500);
		sld.registerStepInformation(4, "type");
		sld.registerStepLocation("[4,3,2,1,0]");
		Thread.sleep(500);
		sld.registerStepFailure("[4,3,2,1,0]");
		Thread.sleep(500);
		sld.registerStepFailure("[3,2,1,0]");
		Thread.sleep(500);
		sld.registerStepFailure("[2,1,0]");
		Thread.sleep(500);
		sld.registerStepFinished("[1,0]");
		Thread.sleep(500);
		sld.registerChartEdge(1, 0, 1, "lexicon");
		Thread.sleep(500);
		sld.registerStepInformation(5, "rule_close");
		sld.registerStepLocation("[5,4,3,2,1,0]");
		Thread.sleep(500);
		sld.registerRuleApplication(6, 0, 2, "head_subject");
		sld.registerStepLocation("[6,5,4,3,2,1,0]");
		Thread.sleep(500);
		sld.registerEdgeDependency(0, 1);
		sld.stop();
	}
}
