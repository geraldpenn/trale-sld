package tralesld.struct.trace;
import java.util.*;

import org.w3c.dom.*;

public class XMLTraceNode
{
    static int number = 0;
    static Map<Integer,XMLTraceNode> nodes = new HashMap<Integer,XMLTraceNode>();
    
    int id;
    int content;
    HashMap<Integer,XMLTraceNode> children;
    XMLTraceNode parent;
    String parentLinkCaption;
    
    public XMLTraceNode(int content, XMLTraceNode parent)
    {
        this.id = number;
        nodes.put(number,this);
        number++;
        this.content = content;
        this.parent = parent;
        parentLinkCaption = "";
        children = new HashMap<Integer,XMLTraceNode>();
    }
    
    public int extendModel(List<Integer> address, int content, String shortDescription)
    {      
        if (address.size() > 0)
        {
            int addressTail = address.remove(address.size() - 1);
            if (children.get(addressTail) == null)
            {
                children.put(addressTail, new XMLTraceNode(-1,this));
            }         
            //children.get(addressTail).parentLinkCaption = ;
            return children.get(addressTail).extendModel(address,content,shortDescription);
        }
        else
        {
            this.content = content;
            this.parentLinkCaption = shortDescription;
            return id;
        }
    }
    
    public void addTreeModelNode(Node parent, Document model)
    {
        /*Node modelNode = new Node(id,content + "");
        modelNode.parentEdgeLabel = parentLinkCaption;
        parent.children.add(modelNode.id);
        model.addNode(modelNode);
        for (XMLTraceNode child : children.values())
        {
            child.addTreeModelNode(modelNode, model);
        }*/
    }
    
}
