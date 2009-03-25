package tralesld.struct.trace;
import java.util.List;

import org.w3c.dom.*;

public class XMLTraceModel
{
    XMLTraceNode root;
    
    public XMLTraceModel()
    {
        root = new XMLTraceNode(-1,null);
    }
    
    public int extendModel(List<Integer> address, int content, String shortDescription)
    {
        return root.extendModel(address,content,shortDescription);
    }
    
    public Document getTreeModel()
    {
        //Document model = new Document();
        /*Node rootNode = new (root.id, root.content + "");
        model.root = root.id;
        model.addNode(rootNode);
        for (XMLTraceNode child : root.children.values())
        {
            child.addTreeModelNode(rootNode, model);
        }*/
        return null;
    }
}
