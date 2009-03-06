import java.util.List;


public class ParseTraceModel
{
    ParseTraceNode root;
    
    public ParseTraceModel()
    {
        root = new ParseTraceNode(-1,null);
    }
    
    public int extendModel(List<Integer> address, int content, String shortDescription)
    {
        return root.extendModel(address,content,shortDescription);
    }
    
    public TreeModel getTreeModel()
    {
        TreeModel model = new TreeModel();
        TreeModelNode rootNode = new TreeModelNode(root.id, root.content + "");
        model.root = root.id;
        model.addNode(rootNode);
        for (ParseTraceNode child : root.children.values())
        {
            child.addTreeModelNode(rootNode, model);
        }
        return model;
    }
}
