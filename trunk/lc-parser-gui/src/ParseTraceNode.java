import java.util.*;

public class ParseTraceNode
{
    static int number = 0;
    static Map<Integer,ParseTraceNode> nodes = new HashMap<Integer,ParseTraceNode>();
    
    int id;
    int content;
    HashMap<Integer,ParseTraceNode> children;
    ParseTraceNode parent;
    String parentLinkCaption;
    
    public ParseTraceNode(int content, ParseTraceNode parent)
    {
        this.id = number;
        nodes.put(number,this);
        number++;
        this.content = content;
        this.parent = parent;
        parentLinkCaption = "";
        children = new HashMap<Integer,ParseTraceNode>();
    }
    
    public int extendModel(List<Integer> address, int content, String shortDescription)
    {      
        if (address.size() > 0)
        {
            int addressTail = address.remove(address.size() - 1);
            if (children.get(addressTail) == null)
            {
                children.put(addressTail, new ParseTraceNode(-1,this));
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
    
    public void addTreeModelNode(TreeModelNode parent, TreeModel model)
    {
        TreeModelNode modelNode = new TreeModelNode(id,content + "");
        modelNode.parentEdgeLabel = parentLinkCaption;
        parent.children.add(modelNode.id);
        model.addNode(modelNode);
        for (ParseTraceNode child : children.values())
        {
            child.addTreeModelNode(modelNode, model);
        }
    }
    
}
