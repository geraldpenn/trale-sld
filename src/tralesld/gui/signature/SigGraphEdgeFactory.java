/**
 * 
 */
package tralesld.gui.signature;

import org.jgrapht.EdgeFactory;

/**
 * @author fdk
 *
 */
public class SigGraphEdgeFactory implements EdgeFactory<SigGraphNode, SigGraphEdge>{

	/* (non-Javadoc)
	 * @see org.jgrapht.EdgeFactory#createEdge(java.lang.Object, java.lang.Object)
	 */
	@Override
	public SigGraphEdge createEdge(SigGraphNode sourceVertex, SigGraphNode targetVertex) {
		return new SigGraphEdge(sourceVertex, targetVertex);
	}
}
