/**
 * 
 */
package tralesld.gui.signature;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.ScrollPane;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;

import javax.swing.JPanel;

import org.jgraph.JGraph;
import org.jgraph.graph.DefaultCellViewFactory;
import org.jgraph.graph.DefaultGraphCell;
import org.jgraph.graph.GraphConstants;
import org.jgraph.graph.GraphLayoutCache;
import org.jgrapht.DirectedGraph;
import org.jgrapht.ext.JGraphModelAdapter;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;

/**
 * VisualSignature contains the method to be used from Trale-SLD to draw a
 * signature graph onto a given JPanel.
 * 
 * @author fdk
 */
public class VisualSignature {

	// private static final Color DEFAULT_BG_COLOR = Color.decode("#FFFFFF");
	private static final Dimension DEFAULT_SIZE = new Dimension(1530, 320);

	private static boolean toproot = true;
	
	static JGraphModelAdapter<SigGraphNode, DefaultEdge> jgmAdapter;
	
	
	
	
	/**
	 * Draw a graph specified by the signature string on the panel.
	 * Root node will be drawn on top.
	 * 
	 * @param signature
	 *            A type hierarchy in TRALE's signature specification file
	 *            format specified at <a href=
	 *            "http://www.ale.cs.toronto.edu/docs/man/ale_trale_man/ale_trale_man-node25.html"
	 *            >http://www.ale.cs.toronto.edu/docs/man/ale_trale_man/
	 *            ale_trale_man-node25.html</a>
	 * @param panel
	 *            A JPanel used for the graphical representation of the type
	 *            hierarchy
	 */
	public static void drawSignature(String signature, JPanel panel) {
		drawSignature(signature, panel, true);
	}
	
	/**
	 * Draw a graph specified by the signature string on the panel.
	 * Position of root node may be set to top or bottom.
	 * 
	 * @param signature
	 *            A type hierarchy in TRALE's signature specification file
	 *            format specified at <a href=
	 *            "http://www.ale.cs.toronto.edu/docs/man/ale_trale_man/ale_trale_man-node25.html"
	 *            >http://www.ale.cs.toronto.edu/docs/man/ale_trale_man/
	 *            ale_trale_man-node25.html</a>
	 * @param panel
	 *            A JPanel used for the graphical representation of the type
	 *            hierarchy
	 * @param toproot
	 * 				If set to true, the root will be drawn at the top of the graph, else it
	 * 				it will be drawn at the bottom.  Default is 'true'.
	 */
	public static void drawSignature(String signature, JPanel panel, boolean toproot) {

		VisualSignature.toproot = toproot;
		
		/*
		 * A TRALE signature is a directed graph. We will use the libs JGraphT
		 * and JGraph. JGraphT is for the actual graph data structure and
		 * algorithms, JGraph is for visualizing the graph data.
		 * 
		 * Maybe we can avoid JGraphT.
		 */

		SigDirectedGraph siggraph = new SigDirectedGraph();

		/*
		 * ----------------------------------------------------------------------
		 * Create the signature graph by adding vertices and edges
		 * to the DirectedGraph-Object.
		 */

		/*
		 * 1. Get the type nodes and generate the vertices
		 */

		ArrayList<String> siglines = Tools.getSignatureLines(Tools.stringToLinesArrayList(signature));

		// get the indent string
		String indentString = null;
		if (Tools.startsWithTabOrBlank(siglines.get(1))) {
			indentString = Tools.getIndentString(siglines.get(1));
		} else {
			System.err.println("Subtypes must be indented with at least one tab or blank.");
			System.exit(-1);
			// 
		}

		// generate an ArrayList of SigGraphNode-objects
		ArrayList<SigGraphNode> rankedNodes = new ArrayList<SigGraphNode>();
		for (int i = 0; i < siglines.size(); i++) {
			String sigline = siglines.get(i);

			int leadingprefixes = Tools.countLeadingPrefixes(sigline, indentString);
			sigline = Tools.stripLeadingPrefixes(sigline, indentString);
			if (!Tools.startsWithTabOrBlank(sigline)) {
				/*
				 * parse --> type and optional attribute-value pairs
				 */

				String type = Tools.getTypeFromSigString(sigline);
				ArrayList<AVPair> avpairs = Tools.getAVPairsFromSigString(sigline);

				/*
				 * We take the number of leading prefixes as a preliminary rank.
				 * Actually this is not needed (anymore), because we assign
				 * ranks later on in the algorithm for drawing the graph. But it
				 * doesn't do any harm, so we keep it like this. Maybe get rid
				 * of it later.
				 */

				SigGraphNode mygraphnode = new SigGraphNode(type, leadingprefixes);
				mygraphnode.setAvpairs(avpairs);

				/*
				 * We give all our nodes unique IDs
				 */
				mygraphnode.setId(i);

				// add the node
				rankedNodes.add(mygraphnode);

			} else {
				/*
				 * The line starts with a tab or a blank --> Not stripped away
				 * --> Inconsistency
				 */
				System.err.println("Inconsistent use of tabs and/or blanks for indentation.");
				System.exit(-1);
			}
		}

		/*
		 * 2. Add all the type nodes as vertices to our graph
		 */
		for (int i = 0; i < rankedNodes.size(); i++) {
			SigGraphNode rankedNode = rankedNodes.get(i);
			System.out.println(rankedNode.toString());
			siggraph.addNode(rankedNode);
		}

		/*
		 * 3. Add edges
		 * 
		 * 
		 * Assume a type k with n = rank(k), n > 0 and i = id(k). Then insert an
		 * edge from k's parent to k.
		 * 
		 * k's parent type e is the type with the maximum id(e) < id(k), with
		 * rank(e) = rank(k) - 1
		 * 
		 * 
		 * + Index 0 is bot -- there's no need to search for edges to bo
		 * + Every node except the root has at least rank 1
		 * + Every node except the root has at least 1 parent.
		 *   (At this point they all have exactly 1 parent)
		 */
		for (int i = 1; i < rankedNodes.size(); i++) {
			SigGraphNode targetNode = rankedNodes.get(i);
			SigGraphNode sourceNode = null;

			// Find parent node
			for (int j = i - 1; j >= 0; j--) {
				sourceNode = rankedNodes.get(j);
				if (sourceNode.getRank() == targetNode.getRank() - 1) {
					break;
				}
			}

			siggraph.addEdge(new SigGraphEdge(sourceNode, targetNode));
		}

		/*
		 * Now we have a graph with type *nodes* with unique IDs, but there may
		 * be more than one node for a given type.
		 * 
		 * We have to merge those nodes, resulting in 1 type node per type. Of
		 * course we have to adjust the edges. Note: After this step, there
		 * might be gaps in the IDs, but we don't care.
		 */

		ArrayList<SigGraphNode> nodes = siggraph.getNodes();

		/*
		 * For all nodes, collect the type in a list. If the list contains the
		 * type, we have to merge nodes.
		 */

		ArrayList<String> seentypes = new ArrayList<String>();
		ArrayList<SigGraphNode> uniquenodes = new ArrayList<SigGraphNode>();

		for (int j = 0; j < nodes.size(); j++) {

			SigGraphNode myvertex = nodes.get(j);

			String vertextype = myvertex.getType();

			if (!seentypes.contains(vertextype)) {
				seentypes.add(vertextype);
				uniquenodes.add(myvertex);

			}
			else {

				/*
				 * We've seen this type before - merge nodes
				 */

				// Find node with that type
				SigGraphNode uniquevertex = null;
				for (int i = 0; i < uniquenodes.size(); i++) {
					uniquevertex = uniquenodes.get(i);
					if (uniquevertex.getType().equals(vertextype)) {
						// That's the one!
						break;
					}
				}

				/*
				 * Replace all edges with target vertex by edges with target
				 * uniquevertex,
				 */
				ArrayList<SigGraphEdge> inEdges = siggraph
						.getIncomingEdgesOf(myvertex);
				if (inEdges.size() > 0) {
					for (int i = 0; i < inEdges.size(); i++) {
						SigGraphEdge inEdge = inEdges.get(i);
						inEdge.setTargetNode(uniquevertex);
					}
				}

				/*
				 * Replace all edges with source vertex by edges with source
				 * uniquevertex,
				 */
				ArrayList<SigGraphEdge> outEdges = siggraph
						.getOutgoingEdgesOf(myvertex);
				if (outEdges.size() > 0) {
					for (int i = 0; i < outEdges.size(); i++) {
						SigGraphEdge outEdge = outEdges.get(i);
						outEdge.setSourceNode(uniquevertex);
					}
				}

				/*
				 * Merge features, i.e. add the features of the soon to be
				 * deleted node to the node that we keep.
				 * 
				 * TODO check for feature consistency?
				 */

				ArrayList<AVPair> avpairs = myvertex.getAvpairs();

				if (avpairs.size() > 0) {
					for (int i = 0; i < avpairs.size(); i++) {
						AVPair avpair = avpairs.get(i);
						uniquevertex.getAvpairs().add(avpair);
					}
				}

				/*
				 * delete old node
				 */

				siggraph.removeNode(myvertex);

			}
		}

		/*
		 * ----------------------------------------------------------------------
		 * 
		 * Now we have a directed graph with our signature data. Next thing is
		 * to calculate the layout.
		 * 
		 * First we remove cycles by reverting appropriate edges, then we
		 * calculate the directed *acyclic* graph. Of course we have to
		 * re-construct the reverted edges in the end, so we have to store which
		 * edges were reverted.
		 * 
		 * 
		 * Drawing of DAGs:
		 * 1. assign ranks to nodes ( --> y-coordinates)
		 * 2. sort nodes within the rank (minimize crossings)
		 * 3. find good places in rank ( --> x-coordinates)
		 * 
		 * In order to do 2 in the way we intend to do it, there mustn't exist
		 * edges spanning more than 1 rank. We achieve this by inserting dummy
		 * nodes.
		 */

		siggraph.removeCycles();

		siggraph.assignRanks();

		siggraph.insertDummyNodes();

		siggraph.minimizeCrossings();

		/*
		 * TODO Under construction
		 */
		// siggraph.setHorizontalRanks();
		/*
		 * ----------------------------------------------------------------------
		 */

		/*
		 * Now we construct a JGraphT graph from our own graph
		 */
		DirectedGraph<SigGraphNode, DefaultEdge> g = new DefaultDirectedGraph<SigGraphNode, DefaultEdge>(DefaultEdge.class);
		for (int i = 0; i < siggraph.getNodes().size(); i++) {
			SigGraphNode mynode = siggraph.getNodes().get(i);
			g.addVertex(mynode);
		}
		for (int i = 0; i < siggraph.getEdges().size(); i++) {
			SigGraphEdge myedge = siggraph.getEdges().get(i);
			g.addEdge(myedge.getSourceNode(), myedge.getTargetNode());
		}

		/*
		 * Create the adapter for JGraph. This adapter extends JGraph's
		 * DefaultGraphModel, which implements its GraphModel.
		 */
		jgmAdapter = new JGraphModelAdapter<SigGraphNode, DefaultEdge>(g);

		for (int i = 0; i < siggraph.getNodes().size(); i++) {
			SigGraphNode node = siggraph.getNodes().get(i);
			setVertexAt(node, i * 100, node.getRank() * 150);
		}

		// DefaultEdge edge = g.getEdge(rankedNodes.get(0), rankedNodes.get(1));

		// DefaultGraphCell cell = jgmAdapter.getEdgeCell(edge);
		// get the attributes of the cell of the given vertex
		// Map attr = cell.getAttributes();

		// GraphConstants.setDisconnectable(attr, false);

		// String fontname = "Verdana";
		// String fontname = "Arial";
		// String fontname = "Times";
		// String fontname = "Courier";
		// int fontstyle = Font.PLAIN;
		// int fontsize = 12;
		// GraphConstants.setFont(attr, new Font(fontname, fontstyle,
		// fontsize));

		// // get the bounds-attribute from the attributes
		// Rectangle2D b = GraphConstants.getBounds(attr);
		//        
		// // edit the bounds-attribute
		// b.setRect(x, y, b.getWidth(), b.getHeight());
		//        
		// // set with the new bounds
		// GraphConstants.setBounds(attr, b);
		//
		// GraphConstants.setFont(attr, new Font(fontname, fontstyle,
		// fontsize));

		// Map cellAttr = new HashMap();
		// cellAttr.put(cell, attr);

		// jgmAdapter.edit(cellAttr, null, null, null);

		GraphLayoutCache view = new GraphLayoutCache(jgmAdapter, new DefaultCellViewFactory());

		// create a visualization using JGraph, via the adapter and the view
		JGraph jgraph = new JGraph(jgmAdapter, view);

		
		adjustDisplaySettings(jgraph);

		/*
		 * Put the graphics in a scrollable area, and add the whole thing to the
		 * given JPanel
		 */
		ScrollPane sp = new ScrollPane();
		sp.add(jgraph);
		panel.add(sp);
	}

	private static void adjustDisplaySettings(JGraph jg) {
		jg.setPreferredSize(DEFAULT_SIZE);
		// Color c = DEFAULT_BG_COLOR;
		// jg.setBackground(c);
		//        

		// jg.getAttributes(arg0);
		//        
		// Font arg1;
		// Map arg0;
		// GraphConstants.setFont(arg0, arg1);

	}

	private static void setVertexAt(Object vertex, int x, int y) {

		// String fontname = "Verdana";
		// String fontname = "Courier";
		// String fontname = "Times";
		// String fontname = "Helvetica";
		// String fontname = "Times New Roman";
		// int fontstyle = Font.PLAIN;
		// int fontsize = 18;

		// System.out.println("Position vertex: " + vertex.toString());

		// get the cell of the given vertex
		DefaultGraphCell cell = jgmAdapter.getVertexCell(vertex);
 
		
		// get the attributes of the cell of the given vertex
		Map attr = cell.getAttributes();

		// get the bounds-attribute from the attributes
		Rectangle2D b = GraphConstants.getBounds(attr);

		// edit the bounds-attribute
		b.setRect(x, y, b.getWidth(), b.getHeight());

		// set with the new bounds
		GraphConstants.setBounds(attr, b);

		// GraphConstants.setFont(attr, new Font(fontname, fontstyle, fontsize));

		// GraphConstants.setInset(attr, fontsize/2);
		GraphConstants.setInset(attr, 10);

		// GraphConstants.setResize(attr, true);
		GraphConstants.setAutoSize(attr, true);

		GraphConstants.setBackground(attr, new Color(222, 222, 222));
		GraphConstants.setForeground(attr, new Color(0, 0, 0));

		Map cellAttr = new HashMap();
		cellAttr.put(cell, attr);
		jgmAdapter.edit(cellAttr, null, null, null);
	}

}
