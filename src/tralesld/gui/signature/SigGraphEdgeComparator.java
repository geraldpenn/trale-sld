/**
 * 
 */
package tralesld.gui.signature;

import java.util.Comparator;


/**
 * Comparator class for SigGraphEdge objects.
 * <p>
 * Order ascending with 'ASC' or descending with 'DESC'
 * <p>
 * Order by
 * <ul>
 * <li>source node ID, then target node ID with 'SourceIDTargetID'
 * <li>target node ID, then source node ID with 'TargetIDSourceID'
 * <li>source node position in rank, then target node position in rank with 'SourcePosTargetPos'
 * </ul>
 * Defaults are 'ASC' and 'SourceIDTargetID'.
 * 
 * @author fdk
 */
public class SigGraphEdgeComparator implements Comparator<SigGraphEdge> {

	private String sortby;
	private String order;
	
	public SigGraphEdgeComparator() {
		this("SourceIDTargetID");
	}
	public SigGraphEdgeComparator(String sortby) {
		this(sortby, "ASC");
	}
	public SigGraphEdgeComparator(String sortby, String order) {
		
		if ((sortby.equalsIgnoreCase("SourceIDTargetID"))
				|| (sortby.equalsIgnoreCase("TargetIDSourceID"))
				|| (sortby.equalsIgnoreCase("SourcePosTargetPos"))
//				|| (sortby.equalsIgnoreCase("SourceID")) 
//				|| (sortby.equalsIgnoreCase("TargetID")) 
				) {
			
			this.sortby = sortby;
		}
		else {
			Tools.die("Wrong Argument for Comparator: Must be a valid sort argument");
		}
		
		
		
		if ((order.equalsIgnoreCase("ASC")) || (order.equalsIgnoreCase("DESC"))) {
			this.order = order;
		}
		else {
			Tools.die("Wrong Argument for Comparator: Must be either 'ASC' or 'DESC'");
		}
	}

	
	public int compare (SigGraphEdge e1, SigGraphEdge e2) {

		int comp = 0;
		
		/*
		 * negative: first object comes before the second object
		 * positive: first object comes after the second object
		 * zero: equality
		 */		
		
		if (sortby.equalsIgnoreCase("SourceIDTargetID")) {
			comp = compareSourceIDTargetID(e1, e2);
		}
		else if (sortby.equalsIgnoreCase("TargetIDSourceID")) {
			comp = compareTargetIDSourceID(e1, e2);
		}
		else if (sortby.equalsIgnoreCase("SourcePosTargetPos")) {
			comp = compareSourcePosTargetPos(e1, e2);
		}
		else if (sortby.equalsIgnoreCase("TargetPosSourcePos")) {
			comp = compareTargetPosSourcePos(e1, e2);
		}
		else {
			Tools.die("Unbekannte Vergleichseigenschaft.");
		}
		
		/*
		 * if descending order is wanted, just change the sign
		 */
		if (order.equalsIgnoreCase("DESC")) {
			comp *= -1;
		}
		
		return comp;
	}


	private int compareSourcePosTargetPos(SigGraphEdge e1, SigGraphEdge e2) {
		int comp;

		comp = compareSourcePos(e1, e2);
		if (comp == 0) {
			comp = compareTargetPos(e1, e2);
		}
		return comp;
	}
	private int compareTargetPosSourcePos(SigGraphEdge e1, SigGraphEdge e2) {
		int comp;

		comp = compareTargetPos(e1, e2);
		if (comp == 0) {
			comp = compareSourcePos(e1, e2);
		}
		return comp;
	}
	
	private int compareSourcePos(SigGraphEdge e1, SigGraphEdge e2) {
		int comp;
		/*
		 * ascending
		 */
		if (e1.getSourceNode().getPosInRank() < e2.getSourceNode().getPosInRank()) {
			comp = -1;
		}
		else if (e1.getSourceNode().getPosInRank() == e2.getSourceNode().getPosInRank()) {
			comp = 0;
		}
		else {
			comp = 1;
		}
		return comp;
	}
	private int compareTargetPos(SigGraphEdge e1, SigGraphEdge e2) {
		int comp;
		/*
		 * ascending
		 */
		if (e1.getTargetNode().getPosInRank() < e2.getTargetNode().getPosInRank()) {
			comp = -1;
		}
		else if (e1.getTargetNode().getPosInRank() == e2.getTargetNode().getPosInRank()) {
			comp = 0;
		}
		else {
			comp = 1;
		}
		return comp;
	}
	

	
	
	private int compareSourceIDTargetID(SigGraphEdge e1, SigGraphEdge e2) {
		int comp;

		comp = compareSourceID(e1, e2);
		if (comp == 0) {
			comp = compareTargetID(e1, e2);
		}
		return comp;
	}
	private int compareTargetIDSourceID(SigGraphEdge e1, SigGraphEdge e2) {
		int comp;

		comp = compareTargetID(e1, e2);
		if (comp == 0) {
			comp = compareSourceID(e1, e2);
		}
		return comp;
	}
	
	private int compareSourceID(SigGraphEdge e1, SigGraphEdge e2) {
		int comp;
		/*
		 * ascending
		 */
		if (e1.getSourceNode().getId() < e2.getSourceNode().getId()) {
			comp = -1;
		}
		else if (e1.getSourceNode().getId() == e2.getSourceNode().getId()) {
			comp = 0;
		}
		else {
			comp = 1;
		}
		return comp;
	}
	private int compareTargetID(SigGraphEdge e1, SigGraphEdge e2) {
		int comp;
		/*
		 * ascending
		 */
		if (e1.getTargetNode().getId() < e2.getTargetNode().getId()) {
			comp = -1;
		}
		else if (e1.getTargetNode().getId() == e2.getTargetNode().getId()) {
			comp = 0;
		}
		else {
			comp = 1;
		}
		return comp;
	}
	

	
}
