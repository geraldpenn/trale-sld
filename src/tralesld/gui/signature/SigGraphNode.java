/**
 * 
 */
package tralesld.gui.signature;

import java.util.ArrayList;


/**
 * A SigGraphNode contains the information we need to paint a
 * signature graph's nodes including obvious things like type
 * and a list of attribute value pairs, but also fields for
 * internal calculation of rank etc.
 * 
 * @author fdk
 */
public class SigGraphNode implements Cloneable {

	
	private String type = "";
	private ArrayList<AVPair> avpairs = new ArrayList<AVPair>();

	private int rank;
	private int id;

	private int x = 0;
	private int y = 0;
	
	
	private int prio = 0;
	
	
	private int horizontalRank = -1;
	
	private int minHorizontalRank = -1000000;
	private int maxHorizontalRank = +1000000;
	
	private int posInRank = -1;
	
	
	private double barycenter = -1.0;
	
	
	
	public SigGraphNode(String type, int rank) {
		this.type = type;
		this.rank = rank;
	}
	
	public SigGraphNode(String type) {
		this.type = type;
	}
	
	public SigGraphNode() {}
	
	
	
	public String getType() {
		return type;
	}
	public void setType(String type) {
		this.type = type;
	}

	public int getRank() {
		return rank;
	}
	public void setRank(int rank) {
		this.rank = rank;
	}

	
	public int getX() {
		return x;
	}
	public void setX(int x) {
		this.x = x;
	}
	public int getY() {
		return y;
	}
	public void setY(int y) {
		this.y = y;
	}
	
	public int getPosInRank() {
		return posInRank;
	}
	public void setPosInRank(int posInRank) {
		this.posInRank = posInRank;
	}

	public int getMinHorizontalRank() {
		return minHorizontalRank;
	}
	public void setMinHorizontalRank(int minHorizontalRank) {
		this.minHorizontalRank = minHorizontalRank;
	}
	public int getMaxHorizontalRank() {
		return maxHorizontalRank;
	}
	public void setMaxHorizontalRank(int maxHorizontalRank) {
		this.maxHorizontalRank = maxHorizontalRank;
	}

	public double getBarycenter() {
		return barycenter;
	}
	public void setBarycenter(double barycenter) {
		this.barycenter = barycenter;
	}

	public int getPrio() {
		return prio;
	}
	public void setPrio(int prio) {
		this.prio = prio;
	}

	public int getHorizontalRank() {
		return horizontalRank;
	}
	public void setHorizontalRank(int horizontalRank) {
		this.horizontalRank = horizontalRank;
	}

	public int getId() {
		return id;
	}
	public void setId(int id) {
		this.id = id;
	}


	public ArrayList<AVPair> getAvpairs() {
		return avpairs;
	}
	public void setAvpairs(ArrayList<AVPair> avpairs) {
		this.avpairs = avpairs;
	}

	
	@Override
	public String toString() {
		StringBuffer sb = new StringBuffer();
		
		sb.append("<html>");
		
		sb.append("<i>" + getType() + "</i>");
		
		ArrayList<AVPair> avpairs = getAvpairs();
		for (int i = 0; i < avpairs.size(); i++) {
			AVPair avpair = avpairs.get(i);
			
			sb.append("<br>");
			sb.append(avpair.toStringHTML());
		}

		sb.append("<!-- Rank: " + getRank() + " ID: " + getId() + " PosInRank: " + getPosInRank() + " -->");

		sb.append("</html>");

		return sb.toString();
	}


	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + id;
		result = prime * result + rank;
		result = prime * result + ((type == null) ? 0 : type.hashCode());

		if (avpairs == null) {
			result = prime * result + 0;
		}
		else {
			for (int i = 0; i < avpairs.size(); i++) {
				result = prime * result + avpairs.get(i).hashCode();
			}			
		}
		
		return result;
	}


	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		SigGraphNode other = (SigGraphNode) obj;
		
		
		if (id != other.id)
			return false;

		if (rank != other.rank)
			return false;
		
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;

		
		if (avpairs == null) {
			if (other.avpairs != null)
				return false;
		} else {

			if (avpairs.size() != other.avpairs.size()) {
				return false;
			}
			else {

				// Compare all elements of the ArrayList
				for (int i = 0; i < avpairs.size(); i++) {
					if (!avpairs.get(i).equals(other.avpairs.get(i))) {
						return false;
					}
				}

			}
		}
		
		return true;
	}
	

	
	/*
	 * clone
	 */
	@Override
	public Object clone() {
		
		SigGraphNode tmp = null;
		try {
			tmp = (SigGraphNode) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
		}

		/*
		 * primitive types are cloned correctly by Object.clone()
		 * immutable reference types (e.g String) are shared, but that's ok because they're immutable.
		 * arrays of these two sorts of types are cloned correctly by the array[] clone()-method.
		 * 
		 * but we have to care for the fields with other types
		 */
		
		
		/*
		 * deep copy avpairs
		 */
		ArrayList<AVPair> tmpavpairs = new ArrayList<AVPair>();
		for (int i = 0; i < avpairs.size(); i++) {
			AVPair avpair = avpairs.get(i);
			tmpavpairs.add((AVPair) avpair.clone());
		}
		tmp.setAvpairs(tmpavpairs);
		
		return tmp;
	}	
	
}
