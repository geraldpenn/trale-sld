/**
 * 
 */
package tralesld.gui.signature;

import org.jgrapht.graph.DefaultEdge;


/**
 * A SigGraphEdge represents a directed connection between two SigGraphNodes.
 * 
 * @author fdk
 *
 */
public class SigGraphEdge extends DefaultEdge implements Cloneable {


	private SigGraphNode sourceNode;
	private SigGraphNode targetNode;
	private boolean reverted = false;
	
	public SigGraphEdge(SigGraphNode source, SigGraphNode target) {
		sourceNode = source;
		targetNode = target;
	}
	
	public SigGraphNode getSourceNode() {
		return sourceNode;
	}
	public void setSourceNode(SigGraphNode source) {
		sourceNode = source;
	}
	
	public SigGraphNode getTargetNode() {
		return targetNode;
	}
	public void setTargetNode(SigGraphNode target) {
		targetNode = target;
	}

	public boolean isReverted() {
		return reverted;
	}

	public void setReverted(boolean reverted) {
		this.reverted = reverted;
	}
	
	
	

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result
				+ ((sourceNode == null) ? 0 : sourceNode.hashCode());
		result = prime * result
				+ ((targetNode == null) ? 0 : targetNode.hashCode());
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
		SigGraphEdge other = (SigGraphEdge) obj;
		if (sourceNode == null) {
			if (other.sourceNode != null)
				return false;
		} else if (!sourceNode.equals(other.sourceNode))
			return false;
		if (targetNode == null) {
			if (other.targetNode != null)
				return false;
		} else if (!targetNode.equals(other.targetNode))
			return false;
		return true;
	}
	
	/*
	 * clone
	 */
	@Override
	public Object clone() {
		
		SigGraphEdge tmp = null;
		tmp = (SigGraphEdge) super.clone();

		/*
		 * primitive types are cloned correctly by Object.clone()
		 * immutable reference types (e.g String) are shared, but that's ok because they're immutable
		 * arrays of these two sorts of types are cloned correctly by the array's clone()-method.
		 * 
		 * but we have to care for the fields with other types
		 */
		
		
		/*
		 * deep copy the two nodes
		 */
		
		tmp.setSourceNode((SigGraphNode) getSourceNode().clone());
		tmp.setTargetNode((SigGraphNode) getTargetNode().clone());
		
		
		return tmp;
	}	
	
	
	
	public String toStringDebug() {
		StringBuffer sb = new StringBuffer();
		
		sb.append("Edge: " + getSourceNode().toString() + " --> " + getTargetNode().toString());
		if (isReverted()) {
			sb.append(" (reverted)");
		}
		
		return sb.toString();
	}

    @Override
	public String toString() {
        return null;
    }
	
}
