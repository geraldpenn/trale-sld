/**
 * 
 */
package tralesld.gui.signature;

import java.util.Comparator;


/**
 * Comparator class for SigGraphNode objects.
 * <p>
 * Order ascending with 'ASC' or descending with 'DESC'
 * <p>
 * Order by
 * <ul>
 * <li>node ID with 'ID'
 * <li>priority with 'PRIO'
 * <li>barycenter with 'BARY'
 * <li>position in rank with 'POS'
 * </ul>
 * Defaults are 'ASC' and 'ID'.
 * 
 * @author fdk
 */
public class SigGraphNodeComparator implements Comparator<SigGraphNode> {

	String sortby;
	String order;
	
	public SigGraphNodeComparator() {
		this("ID");
	}
	
	public SigGraphNodeComparator(String sortby) {
		this(sortby, "ASC");
	}
	
	public SigGraphNodeComparator(String sortby, String order) {
		
		if ((sortby.equalsIgnoreCase("ID"))
				|| (sortby.equalsIgnoreCase("PRIO")) 
				|| (sortby.equalsIgnoreCase("BARY")) 
				|| (sortby.equalsIgnoreCase("POS"))
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

	public int compare (SigGraphNode f1, SigGraphNode f2) {

		int comp = 0;
		
		/*
		 * negative: first object comes before the second object
		 * positive: first object comes after the second object
		 * zero: equality
		 */
		
		
		if (sortby.equalsIgnoreCase("ID")) {
			comp = compareID(f1, f2);
		}
		else if (sortby.equalsIgnoreCase("POS")) {
			comp = comparePosInRank(f1, f2);
		}
		else if (sortby.equalsIgnoreCase("BARY")) {
			comp = compareBarycenter(f1, f2);
		}
		else if (sortby.equalsIgnoreCase("PRIO")) {
			comp = comparePrio(f1, f2);
		}
		else {
			Tools.die("Unknown sort attribute.");
		}
		
		/*
		 * if descending order is wanted, just change the sign
		 */
		if (order.equalsIgnoreCase("DESC")) {
			comp *= -1;
		}
		
		return comp;
	}


	private int compareID(SigGraphNode n1, SigGraphNode n2) {
		int comp;
		/*
		 * ascending
		 */
		if (n1.getId() < n2.getId()) {
			comp = -1;
		}
		else if (n1.getId() == n2.getId()) {
			comp = 0;
		}
		else {
			comp = 1;
		}
		return comp;
	}
	
	private int comparePosInRank(SigGraphNode n1, SigGraphNode n2) {
		int comp;
		/*
		 * ascending
		 */
		if (n1.getPosInRank() < n2.getPosInRank()) {
			comp = -1;
		}
		else if (n1.getPosInRank() == n2.getPosInRank()) {
			comp = 0;
		}
		else {
			comp = 1;
		}
		return comp;
	}

	private int compareBarycenter(SigGraphNode n1, SigGraphNode n2) {
		int comp;
		/*
		 * ascending
		 */
		if (n1.getBarycenter() < n2.getBarycenter()) {
			comp = -1;
		}
		else if (n1.getBarycenter() == n2.getBarycenter()) {
			comp = 0;
		}
		else {
			comp = 1;
		}
		return comp;
	}

	private int comparePrio(SigGraphNode n1, SigGraphNode n2) {
		int comp;
		/*
		 * ascending
		 */
		if (n1.getDownPrio() < n2.getDownPrio()) {
			comp = -1;
		}
		else if (n1.getDownPrio() == n2.getDownPrio()) {
			comp = 0;
		}
		else {
			comp = 1;
		}
		return comp;
	}
	
}
