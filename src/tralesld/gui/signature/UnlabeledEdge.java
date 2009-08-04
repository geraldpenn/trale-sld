/**
 * 
 */
package tralesld.gui.signature;

import org.jgrapht.graph.DefaultEdge;

/**
 * @author fdk
 *
 */
public class UnlabeledEdge extends DefaultEdge {

    /**
	 * 
	 */
	private static final long serialVersionUID = -2745383554016870526L;
	
	private final SigGraphNode sourceNode;
    private final SigGraphNode targetNode;

    public UnlabeledEdge(SigGraphNode source, SigGraphNode target) {
        this.sourceNode = source;
        this.targetNode = target;
    }

    public SigGraphNode getSourceNode() {
        return sourceNode;
    }

    public SigGraphNode getTargetNode() {
        return targetNode;
    }

    @Override
	public String toString() {
        return null;
    }

}
