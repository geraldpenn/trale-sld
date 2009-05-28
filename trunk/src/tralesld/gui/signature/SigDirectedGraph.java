/**
 * 
 */
package tralesld.gui.signature;

import java.util.ArrayList;
import java.util.Collections;

/**
 * <p>
 * SigDirectedGraph consists of two ArrayLists, one with SigGraphNode objects,
 * one with SigGraphEdge objects.
 * </p>
 * <p>
 * Apart from the methods for adding and removing nodes and edges etc., there are methods
 * for applying the Sugiyama algorithm for drawing graphs.
 * </p>
 * 
 * @author fdk
 */
public class SigDirectedGraph implements Cloneable {

	/*
	 * There are various ways to implement a directed graph:
	 * - Node list
	 * - Edge list
	 * - Adjacency matrix
	 * - Adjacency list
	 * 
	 * Our graph will consist of a list of Node-objects and a list of Edge-objects
	 */
	
	private ArrayList<SigGraphNode> nodes = new ArrayList<SigGraphNode>();
	private ArrayList<SigGraphEdge> edges = new ArrayList<SigGraphEdge>();
	
	
	public void addNode(SigGraphNode node) {
		nodes.add(node);
	}
	public void removeNode(SigGraphNode node) {
		nodes.remove(node);
	}
	
	
	public void addEdge(SigGraphEdge edge) {
		edges.add(edge);
	}
	public void removeEdge(SigGraphEdge edge) {
		edges.remove(edge);
	}
	
	public ArrayList<SigGraphNode> getNodes() {
		return nodes;
	}
	public void setNodes(ArrayList<SigGraphNode> nodes) {
		this.nodes = nodes;
	}

	public ArrayList<SigGraphEdge> getEdges() {
		return edges;
	}
	public void setEdges(ArrayList<SigGraphEdge> edges) {
		this.edges = edges;
	}


	
	public ArrayList<SigGraphNode> getNodesWithType(String type) {
		ArrayList<SigGraphNode> nodesWithGivenType = new ArrayList<SigGraphNode>();
		
		ArrayList<SigGraphNode> allnodes = this.getNodes();
		
		for (int i = 0; i < allnodes.size(); i++) {
			SigGraphNode allnode = allnodes.get(i);
			if (allnode.getType().equals(type)) {
				nodesWithGivenType.add(allnode);
			}
		}
		
		return nodesWithGivenType;
	}
	

	public ArrayList<SigGraphNode> getNodesWithRank(int rank) {
		ArrayList<SigGraphNode> nodeswithrank = new ArrayList<SigGraphNode>();
		
		for (int i = 0; i < getNodes().size(); i++) {
			SigGraphNode node = getNodes().get(i);
			if (node.getRank() == rank) {
				nodeswithrank.add(node);
			}
		}

		return nodeswithrank;
	}
	
	/**
	 * @param id
	 * @return Node with the given ID or null if no such node exists.
	 * <p>
	 * If there is more than one node with the given ID, die.
	 */
	public SigGraphNode getNodeWithID(int id) {
		
		SigGraphNode nodeWithID = null;
		
		for (int i = 0; i < getNodes().size(); i++) {
			SigGraphNode node = getNodes().get(i);
			if (node.getId() == id) {
				if (nodeWithID != null) {
					/*
					 * TODO Force node IDs in the graph to be unique
					 */
					Tools.die("Duplicate IDs " + id + " in Graph!");
				}
				else {
					nodeWithID = node;
				}
			}
		}
		
		return nodeWithID;
	}
	
	
	public int getMaxNodeID() {
		
		int maxNodeID = -1;
		
		ArrayList<SigGraphNode> nodes = this.getNodes();
		for (int i = 0; i < nodes.size(); i++) {
			SigGraphNode node = nodes.get(i);
			
			int nodeID = node.getId();
			if (nodeID > maxNodeID) {
				maxNodeID = nodeID;
			}
		}
		
		return maxNodeID;
	}
	
	public int getMaxRank() {
		
		int maxRank = -1;
		
		ArrayList<SigGraphNode> nodes = this.getNodes();
		for (int i = 0; i < nodes.size(); i++) {
			SigGraphNode node = nodes.get(i);
			
			int rank = node.getRank();
			if (rank > maxRank) {
				maxRank = rank;
			}
		}
		
		return maxRank;
	}
	
	
	
	public boolean hasIncomingEdge(SigGraphNode node) {
		boolean hasOne = false;
		
		for (int i = 0; i < edges.size(); i++) {
			SigGraphEdge edge = edges.get(i);
			
			if (node.equals(edge.getTargetNode())) {
				hasOne = true;
				break;
			}
		}
		return hasOne;
	}
	public boolean hasOutgoingEdge(SigGraphNode node) {
		boolean hasOne = false;
		
		for (int i = 0; i < edges.size(); i++) {
			SigGraphEdge edge = edges.get(i);
			
			if (node.equals(edge.getSourceNode())) {
				hasOne = true;
				break;
			}
		}
		return hasOne;
	}
	
	

	/**
	 * @param node
	 * @return  A list of edges leading to the node.  The list is empty if there are no such edges.
	 */
	public ArrayList<SigGraphEdge> getIncomingEdgesOf(SigGraphNode node) {
		ArrayList<SigGraphEdge> inEdges = new ArrayList<SigGraphEdge>();
		for (int i = 0; i < edges.size(); i++) {
			SigGraphEdge edge = edges.get(i);
			
			if (node.equals(edge.getTargetNode())) {
				inEdges.add(edge);
			}
		}
		return inEdges;
	}

	/**
	 * @param node
	 * @return A list of edges leading away from the node.  The list is empty if there are no such edges.
	 */
	public ArrayList<SigGraphEdge> getOutgoingEdgesOf(SigGraphNode node) {
		ArrayList<SigGraphEdge> outEdges = new ArrayList<SigGraphEdge>();
		for (int i = 0; i < edges.size(); i++) {
			SigGraphEdge edge = edges.get(i);
			
			if (node.equals(edge.getSourceNode())) {
				outEdges.add(edge);
			}
		}
		return outEdges;
	}
	
	
	
	public ArrayList<SigGraphNode> getRoots() {
		
		/*
		 * Return a list of root nodes of the given graph.
		 * A root node is a node which is no target of any edge.
		 */
		
		ArrayList<SigGraphNode> roots = new ArrayList<SigGraphNode>();
		
		for (int i = 0; i < nodes.size(); i++) {
			SigGraphNode node = nodes.get(i);
			
			if (!hasIncomingEdge(node)) {
				roots.add(node);
			}
		}
		
		return roots;
	}
	
	public void insertDummyNodes() {
		
		/*
		 * Edges need some space when the graph is drawn.
		 * So we insert dummy nodes.
		 */
		
		ArrayList<SigGraphEdge> edges = this.getEdges();
		for (int i = 0; i < edges.size(); i++) {
			SigGraphEdge edge = edges.get(i);
			
			SigGraphNode source = edge.getSourceNode();
			SigGraphNode target = edge.getTargetNode();
			boolean isReverted = edge.isReverted();
			
			int sourcerank = source.getRank();
			int targetrank = target.getRank();
			
			// target ranks are always greater than source ranks in our algorithm
			if ((targetrank - sourcerank) > 1) {
				// edge spans more than 1 rank.  Insert dummy nodes

				for (int j = sourcerank + 1; j < targetrank; j++) {
					
					// insert 1 dummy node

					/*
					 * The new node needs a unique id.  We go for the maxID plus 1.
					 * The rank is given by the loop counter variable.
					 * Note: Node type is the new Node object's default: "" (empty string)
					 * Only dummy nodes have an empty type string.
					 */
					SigGraphNode dummyNode = new SigGraphNode();
					dummyNode.setId(this.getMaxNodeID() + 1);
					dummyNode.setRank(j);
					this.addNode(dummyNode);

					
					SigGraphEdge dummyEdge = new SigGraphEdge(source, dummyNode);
					dummyEdge.setReverted(isReverted);
					this.addEdge(dummyEdge);
					
					edge.setSourceNode(dummyNode);
					source = dummyNode;
				}
				
			}
			
		}// next edge
		
		
		
	}
	
	public void setCoordsY(int rankheight, int offset) {
		
		ArrayList<SigGraphNode> nodes = this.getNodes();
		for (int i = 0; i < nodes.size(); i++) {
			SigGraphNode node = nodes.get(i);
			node.setY((node.getRank() * rankheight) + offset);
		}
	}
	
	public void removeCycles() {
		
		/*
		 * Remove cycles from graph and store the information which edges had been reverted
		 * in order to reconstruct the original graph.
		 * 
		 * We use a quiet simple method:
		 * Give every node an unique integral number.  We already have this: node IDs!
		 * Then for all edges, we keep the direction as is, if the number of the source node
		 * is less than the number of the target node, else we change the direction.
		 * 
		 * Source: Diskrete Mathematik mit 600 Übungsaufgaben Von Martin Aigner Seite 116
		 * http://books.google.de/books?id=pM6_WiVHFUgC&pg=PA116&lpg=PA116&dq=Gerichteten+graph+azyklisch+machen&source=bl&ots=IrJE6nDNnK&sig=Dl5pbwEB2_aEN3zohWryp-EXnCU&hl=de&ei=zszcSYGDKo6JsAaxi7GoDA&sa=X&oi=book_result&ct=result&resnum=4#PPA116,M1
		 * 
		 */
		
		
		for (int i = 0; i < getEdges().size(); i++) {
			SigGraphEdge edge = getEdges().get(i);
			
			SigGraphNode source = edge.getSourceNode();
			SigGraphNode target = edge.getTargetNode();

			if (source.getId() < target.getId()) {
				// keep edge
			}
			else {
				// revert edge
				
				edge.setSourceNode(target);
				edge.setTargetNode(source);
				
				if (edge.isReverted()) {
					edge.setReverted(false);
				}
				else {
					edge.setReverted(true);
				}
				
			}
		}
		
	}
	
	public void assignRanks() {
		SigDirectedGraph assignedgraph = (SigDirectedGraph) this.clone();
		
		/*
		 * Clone graph.
		 * Repeatedly remove all root nodes (i.e. nodes without any incoming edges)
		 * in the clone and all their outgoing edges until there are no root nodes left.
		 * Before removing a node, assign the rank of the node to the corresponding node
		 * in the original graph.
		 */
		
		int rank = 0;
		
		while (true) {
			
			ArrayList<SigGraphNode> roots = assignedgraph.getRoots();
			
			if (roots.size() > 0) {
		
				// remove roots including their edges
				for (int i = 0; i < roots.size(); i++) {
					SigGraphNode root = roots.get(i);
					
					// first remove all the node's outgoing edges
					ArrayList<SigGraphEdge> outedges = assignedgraph.getOutgoingEdgesOf(root);
					for (int j = 0; j < outedges.size(); j++) {
						SigGraphEdge outedge = outedges.get(j);
						assignedgraph.removeEdge(outedge);
					}
					// then remove the node
					assignedgraph.removeNode(root);
		
					ArrayList<SigGraphNode> nodes = this.getNodes();
					for (int j = 0; j < nodes.size(); j++) {
						SigGraphNode node = nodes.get(j);
						if (node.getId() == root.getId()) {
							node.setRank(rank);
							break;
						}
					}
					
				}
				
			}
			else {
				// no more roots
				break;
			}
			
			rank++;
		}
		
	}

	
	private void initPositionsInRank() { 
	
		/*
		 * In order to sort nodes within their rank, (e.g. to minimize crossings)
		 * we have to init the positions.
		 * Positions run from 0 to n
		 */
		
		for (int i = 0; i <= getMaxRank(); i++) {
			ArrayList<SigGraphNode> nodesInRank = getNodesWithRank(i);
			for (int j = 0; j < nodesInRank.size(); j++) {
				SigGraphNode nodeInRank = nodesInRank.get(j);
				nodeInRank.setPosInRank(j);
			}
		}
	}
	
	public void minimizeCrossings() {
		
		/*
		 * In one step, we always look at only two ranks at the same time,
		 * keeping the nodes of one rank fixed, while moving the nodes of the other rank,
		 * minimizing the number of crossings between the two ranks.
		 * 
		 * We sweep the ranks of the graph 1x down and up again, then we count the crossings.
		 * This is repeated until there is no more improvement.
		 */
		
		this.initPositionsInRank(); // initialize the positions of the nodes
		
		int crossings = this.countCrossingsInGraph(); // do a first count
		
		System.out.println("Crossings in graph: " + crossings);
		
		
		boolean reducedCrossings = true;
		while (reducedCrossings) {
			
			/*
			 * keep a clone of the original graph, in case the new one has more crossings.
			 * sort nodes 1x down and back up again.
			 */
			SigDirectedGraph oldgraph = (SigDirectedGraph) this.clone();
			this.sortNodePositions();
			
			/*
			 * count crossings
			 */
			int newcrossings = this.countCrossingsInGraph();
			System.out.println("New crossings: " + newcrossings);
			
			if (newcrossings < crossings) {
				crossings = newcrossings;
				reducedCrossings = true;
			}
			else {
				reducedCrossings = false;
				// restore the original graph
				this.setNodes(oldgraph.getNodes());
				this.setEdges(oldgraph.getEdges());
			}
		}

		
	}
	
	private void sortNodePositions() {
		
		/*
		 * We work from the top rank down and back up again, always looking at two ranks of nodes.
		 * One rank's order is fixed, the other one's will be sorted.
		 * 
		 * On Barycenter algorithm problems:
		 * http://www.ijicis.net/Vol2_No2%20No2.pdf
		 * 
		 * Anyway, we use the Barycenter method.
		 * 
		 * First we keep the 0th rank's nodes fixed, reordering the nodes of rank 1
		 */
		
		System.out.println("sort down and up again");
		
		for (int i = 0; i < this.getMaxRank(); i++) {
			
			/*
			 * keep positions of rank i, reorder positions of rank i+1
			 */
			
			ArrayList<SigGraphNode> freenodes = this.getNodesWithRank(i+1);
			for (int j = 0; j < freenodes.size(); j++) {
				SigGraphNode freenode = freenodes.get(j);
				
				// calc the barycenter value for this node
				ArrayList<SigGraphEdge> incomingedges = this.getIncomingEdgesOf(freenode);
				
				double bary = 0.0;
				
				if (incomingedges.size() > 0) {
					for (int k = 0; k < incomingedges.size(); k++) {
						SigGraphEdge incomingedge = incomingedges.get(k);
						bary += incomingedge.getSourceNode().getPosInRank();
					}
					bary = (bary/incomingedges.size());
				}
				
				freenode.setBarycenter(bary);
				
			}
			
			/*
			 * now we have all barycenter values for the free nodes.
			 * sort and assign positions
			 */
			
			Collections.sort(freenodes, new SigGraphNodeComparator("BARY"));
			for (int j = 0; j < freenodes.size(); j++) {
				SigGraphNode freenode = freenodes.get(j);
				freenode.setPosInRank(j);
			}
			
		}
		
		
		// TODO don't copy code.
		
		for (int i = this.getMaxRank(); i > 0; i--) {
			
			/*
			 * keep positions of rank i, reorder positions of rank i-1
			 */
			
			ArrayList<SigGraphNode> freenodes = this.getNodesWithRank(i-1);
			for (int j = 0; j < freenodes.size(); j++) {
				SigGraphNode freenode = freenodes.get(j);
				
				// calc the barycenter value for this node
				ArrayList<SigGraphEdge> outgoingedges = this.getOutgoingEdgesOf(freenode);
				
				double bary = 0.0;
				
				if (outgoingedges.size() > 0) {
					for (int k = 0; k < outgoingedges.size(); k++) {
						SigGraphEdge incomingedge = outgoingedges.get(k);
						bary += incomingedge.getSourceNode().getPosInRank();
					}
					bary = (bary/outgoingedges.size());
				}
				
				freenode.setBarycenter(bary);
				
			}
			
			/*
			 * now we have all barycenter values for the free nodes.
			 * sort and assign positions
			 */
			
			Collections.sort(freenodes, new SigGraphNodeComparator("BARY"));
			for (int j = 0; j < freenodes.size(); j++) {
				SigGraphNode freenode = freenodes.get(j);
				freenode.setPosInRank(j);
			}
			
		}
	
		
		
	}
	
	public void setHorizontalRanks() {
		
		/*
		 * Similar to the minimizeCrossings() method, we sweep up and down the ranks of the graph,
		 * keeping one rank fixed while we move the nodes of the other rank.
		 * This time, we assign the nodes of each rank initial x-positions from 1 to n
		 * 
		 * First we assign priorities to the nodes.  For all nodes except dummy nodes, this is
		 * the number of neighbours.  A neighbour of a node n is a node which is connected to
		 * n by an edge.
		 * Dummy nodes with another dummy node as the only neighbour get highest priority -- we
		 * use the max number of possible neighbours plus 1
		 * 
		 * 
		 * Then we work on the nodes, starting with the highest priority node, placing them at
		 * their best x-position, but with respect to some constraints:
		 * 
		 * - order of nodes may not be changed.
		 * 
		 * - nodes with higher priority may only be moved if space is needed.  If there
		 *   are two nodes with higher priority, and either the one or the other has to
		 *   be moved when space is needed, move the one with lower priority.
		 * 
		 * 
		 * Note: These x-positions aren't pixel coordinates, they are horizontal ranks
		 */
		
		
		System.out.println("find positions down and up again");
		
		for (int i = 0; i < this.getMaxRank(); i++) {
		
			System.out.println("Rank " + i + " and " + (i+1) + ":");

			ArrayList<SigGraphNode> moveNodes = this.getNodesWithRank(i+1);
			ArrayList<SigGraphNode> nodesbyprio = new ArrayList<SigGraphNode>();
			for (int j = 0; j < moveNodes.size(); j++) {
				moveNodes.get(j).setHorizontalRank(j+1);	// init the x-positions, they run from 1 to n
				SigGraphNode node = (SigGraphNode) moveNodes.get(j).clone();
				nodesbyprio.add(node);
			}
			assignNodePriorities(nodesbyprio, this.getNodesWithRank(i).size()+1);
			
			/*
			 * find places for the nodes in order of priority descending
			 */
			for (int j = 0; j < nodesbyprio.size(); j++) {
				int moveNodeID = nodesbyprio.get(j).getId();
				
				// get the node with this ID
				SigGraphNode moveNode = this.getNodeWithID(moveNodeID);
				
				/*
				 * Now we try to find a place for the node.
				 * Best place would be calculated this way:
				 * Take all the edges leading to our node, find the middle between the
				 * leftmost node and the rightmost node. If the resulting position is
				 * no integral value, pick one of the nearest two nodes.
				 * (left or right, we'll decide on a strategy later)
				 */
				
				int left = this.getNodesWithRank(i).size()+1;
				int right = 0;
				
				ArrayList<SigGraphEdge> incomingedges = this.getIncomingEdgesOf(moveNode);
				for (int k = 0; k < incomingedges.size(); k++) {
					SigGraphEdge incomingedge = incomingedges.get(k);
					
					SigGraphNode src = incomingedge.getSourceNode();
					int pos = src.getHorizontalRank();
					if (pos < left) {
						left = pos;
					}
					if (pos > right) {
						right = pos;
					}
				}

				int mid = Tools.intmid(left, right);
				
				/*
				 * mid *would* be the best position, but we have to see
				 * if this position is really available.
				 */

				/* 
				 * TODO
				 * First we check if the wanted spot is still available.
				 * To do this, we have to consider all previously set nodes, i.e.
				 * all with higher priority.
				 */

				boolean occupied = false;
				for (int k = 0; k < j; k++) {
					if (this.getNodeWithID(nodesbyprio.get(k).getId()).getHorizontalRank() == mid) {
						occupied = true;
						break;
					}
				}
				
				if (!occupied) {
					/*
					 * This spot is available -- put the node here, but without
					 * changing the horizontal order of the nodes.
					 * 
					 */
					moveNode.setHorizontalRank(mid);
					
					
					
					
					
					
					
					
					
				}
				else {
					/*
					 * Look for some other place.
					 * 
					 */
					
					
					
					
				}
				
				
				
				
				
				
			}
			
			
		}
		
	}
	
	
	private void assignNodePriorities(ArrayList<SigGraphNode> nodesByPrio, int dummyprio) {
		
		for (int j = 0; j < nodesByPrio.size(); j++) {
			SigGraphNode freenode = nodesByPrio.get(j);
			
			if (freenode.getType().equals("")) {
				/* 
				 * A dummy node.  As dummy nodes are inserted into edges,
				 * there is always exactly one incoming and one outgoing edge.
				 */
				if(this.getIncomingEdgesOf(freenode).get(0).getSourceNode().getType().equals("")) {
					/*
					 * The node above is a dummy node, too.
					 * Assign highest priority.
					 */
					freenode.setPrio(dummyprio);
				}
				else {
					freenode.setPrio(1); // there's only one incoming edge
				}
			}
			else {
				// not a dummy node, assign prio according to edges
				freenode.setPrio(this.getIncomingEdgesOf(freenode).size());
			}
		}

		/*
		 * now sort nodes by prio, highest first
		 */
		Collections.sort(nodesByPrio, new SigGraphNodeComparator("PRIO", "DESC"));

	}
	
	
	
	
	private int countCrossingsInGraph() {
		int crossings = 0;
		
		/*
		 * The total of crossings is the sum of the crossings between rank r and rank r+1
		 */
		
		for (int i = 0; i <= getMaxRank()-1; i++) {
			crossings += countCrossingsInRankBJM(i);
		}
		
		return crossings;
	}
	
	private int countCrossingsInRankBJM(int r) {
		
//		System.out.println("Counting crossings from rank " + r + " to rank " + (r+1));
		
		int count = 0;
		
		/*
		 * Assign ascending node numbers from the left-most node in rank r
		 * to the right-most node in rank r+1, going first left-to-right,
		 * then top-to-bottom.
		 * 
		 * Get all edges leading from rank r to rank r+1
		 * Sort them lexicographically, first by source, then by target
		 * Remove sources, leave targets.
		 * 
		 * For all targets t, count the number of targets left from t and greater than t
		 * Sum up these counts
		 */
		
		int posOffset = getNodesWithRank(r).size();
		
//		System.out.println("Nodes in rank " + r + ": " + posOffset);

		ArrayList<SigGraphEdge> tmpedges = new ArrayList<SigGraphEdge>();
		
		
		for (int i = 0; i < getEdges().size(); i++) {
			SigGraphEdge edge = getEdges().get(i);
			if ((edge.getSourceNode().getRank()==r) && (edge.getTargetNode().getRank()==r+1)) {
				
				/*
				 * This is an edge we're interested in
				 */
				
//				System.out.println("Working for edge " + edge.toString());
				
				SigGraphNode tmpSource = new SigGraphNode();
				tmpSource.setId(edge.getSourceNode().getPosInRank());

				SigGraphNode tmpTarget = new SigGraphNode();
				tmpTarget.setId(edge.getTargetNode().getPosInRank()+posOffset);
				
				SigGraphEdge tmpedge = new SigGraphEdge(tmpSource, tmpTarget);
				
//				System.out.println("Adding new edge: " + tmpedge.toString());
//				System.out.println();
				tmpedges.add(tmpedge);
			}
		}
		
		/*
		 * Now we have an ArrayList of edges, connecting nodes with IDs set to the numbers
		 * given by their positions (+offset).
		 */
		
		Collections.sort(tmpedges, new SigGraphEdgeComparator("SourceIDTargetID"));
		
		/*
		 * Now the edges are sorted first by sourceID then by targetID - these IDs correspond to the
		 * numbering.
		 */
		
//		System.out.println("Sorted edges:");
		for (int i = 0; i < tmpedges.size(); i++) {
			SigGraphEdge tmpedge = tmpedges.get(i);
//			System.out.println(tmpedge.toString());
		}
		
		
		/*
		 * put the targetIDs in a new ArrayList in the same order
		 */
		ArrayList<Integer> targetIDs = new ArrayList<Integer>();
		
		for (int i = 0; i < tmpedges.size(); i++) {
			SigGraphEdge tmpedge = tmpedges.get(i);
			targetIDs.add(tmpedge.getTargetNode().getId());
		}


		for (int i = 0; i < targetIDs.size(); i++) {
			Integer targetID = targetIDs.get(i);
			
			for (int j = 0; j < i; j++) {
				Integer tid = targetIDs.get(j);
				
				if (tid > targetID) {
					count++;
				}
				
			}
		}
		
		
		
		return count;
	}
	
	
	
	
	
	
	
	
	public boolean hasCycle() {
		
		/*
		 * Return true if the directed graph is cyclic.
		 * 
		 * Algorithm:
		 * Repeatedly remove all root nodes (i.e. nodes without any incoming edges)
		 * and all their outgoing edges until there are no root nodes left.
		 * If the graph still contains any nodes, there is at least one cycle.
		 */

		boolean hasCycle;
		
		SigDirectedGraph checkgraph = (SigDirectedGraph) this.clone();
		
		while (true) {
			
			ArrayList<SigGraphNode> roots = checkgraph.getRoots();
			
			if (roots.size() > 0) {
		
				// remove roots including their edges
				for (int i = 0; i < roots.size(); i++) {
					SigGraphNode root = roots.get(i);
					
					// first remove all the node's outgoing edges
					ArrayList<SigGraphEdge> outedges = checkgraph.getOutgoingEdgesOf(root);
					for (int j = 0; j < outedges.size(); j++) {
						SigGraphEdge outedge = outedges.get(j);
						checkgraph.removeEdge(outedge);
					}
					// then remove the node
					checkgraph.removeNode(root);
					
				}
				
			}
			else {
				// no more roots
				break;
			}
		}
		
		if (checkgraph.getNodes().size() > 0) {
			hasCycle = true;
		}
		else {
			hasCycle = false;
		}
		
		return hasCycle;
	}
		
	/*
	 * clone
	 */
	@Override
	public Object clone() {
		
		SigDirectedGraph tmp = null;
		try {
			tmp = (SigDirectedGraph) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}

		/*
		 * deep copy nodes
		 */
		ArrayList<SigGraphNode> tmpnodes = new ArrayList<SigGraphNode>();
		for (int i = 0; i < nodes.size(); i++) {
			SigGraphNode node = nodes.get(i);
			tmpnodes.add((SigGraphNode) node.clone());
		}
		tmp.setNodes(tmpnodes);
		
		/*
		 * deep copy edges
		 */
		ArrayList<SigGraphEdge> tmpedges = new ArrayList<SigGraphEdge>();
		for (int i = 0; i < edges.size(); i++) {
			SigGraphEdge edge = edges.get(i);
			tmpedges.add((SigGraphEdge) edge.clone());
		}
		tmp.setEdges(tmpedges);
		
		return tmp;
	}
	
	
	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		
		/*
		 * list of nodes
		 */
		
		for (int i = 0; i < nodes.size(); i++) {
			SigGraphNode node = nodes.get(i);
			sb.append(node.toString() + "\n");
		}
		for (int i = 0; i < edges.size(); i++) {
			SigGraphEdge edge = edges.get(i);
			sb.append(edge.toString() + "\n");
		}
		
		return sb.toString();
	}
	
	
	
	
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {

		
		/*
		 * Tests
		 */
		
		SigDirectedGraph mdg;
//		
//		mdg = new MyDirectedGraph();
//		
//		
//		SigGraphNode node0 = new SigGraphNode();
//		node0.setType("Type0");
//		node0.setId(0);
//		SigGraphNode node1 = new SigGraphNode();
//		node1.setType("Type1");
//		node1.setId(1);
//		
//		mdg.addNode(node0);
//		mdg.addNode(node1);
//		
//		SigGraphEdge edge01 = new SigGraphEdge(node0, node1);
//		SigGraphEdge edge10 = new SigGraphEdge(node1, node0);
//		
//		mdg.addEdge(edge01);
//		mdg.addEdge(edge10);

		
		
//		String filename = "data/dg03.txt";
//		String filename = "data/dg_8crossings.txt";
		String filename = "data/dg_testxpos01.txt";
		
		mdg = Tools.constructDirectedGraphFromFile(filename);
		
		
		
		if (mdg.hasCycle()) {
			System.out.println("Graph has at least one cycle.");
			System.out.println(mdg.toString());
			
			
			System.out.println("Making it acyclic now.");
			
			mdg.removeCycles();
			
			System.out.println(mdg.toString());
			
		}
		else {
			System.out.println("Graph has no cycles.");
			System.out.println(mdg.toString());
		}
		
		
		System.out.println("Assigned Ranks:");
		mdg.assignRanks();
		System.out.println(mdg.toString());
		

		System.out.println("Inserted dummy nodes if needed:");
		mdg.insertDummyNodes();
		System.out.println(mdg.toString());
		
		System.out.println("Minimize crossings:");
		mdg.minimizeCrossings();
		System.out.println(mdg.toString());
		
		
		System.out.println("Place nodes:");
		mdg.setHorizontalRanks();
		System.out.println(mdg.toString());
		
	}


}
