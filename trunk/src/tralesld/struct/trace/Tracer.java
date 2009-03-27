package tralesld.struct.trace;

import java.util.*;

public class Tracer
{
    public XMLTraceModel traceModel;
    
    public Tracer()
    {
        traceModel = new XMLTraceModel();
    }
    
    public XMLTraceNode registerStep(List<Integer> stackList, int content, String shortDescription)
    {
        return traceModel.extendModel(stackList, content, shortDescription);
    }
    
    public XMLTraceNode registerStepAsChildOf(int parentID , int stepID, int content, String shortDescription)
    {
    	XMLTraceNode parentNode = XMLTraceNode.nodes.get(parentID);
    	if (parentNode == null) parentNode = traceModel.root;
    	XMLTraceNode childNode = new XMLTraceNode(stepID,stepID,parentNode, traceModel.modelDOM);
    	childNode.setParentLinkCaption(shortDescription);
    	parentNode.getChildren().put(stepID, childNode);       
        return childNode;
    }
}
