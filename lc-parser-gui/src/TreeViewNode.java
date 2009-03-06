import java.util.*;
import java.awt.*;

public class TreeViewNode 
{
	TreeModelNode modelNode;
    int id;
    int parent;
    ArrayList<Integer> children;
    String tag;
    Color color;
    Color edgeTagColor;
    int x;
    int y;
    int subTreeWidth;
    String edgeTag;
    String edgeDir;
	String edgeType;
    
    public TreeViewNode(TreeModelNode modelNode, int parent, ArrayList<Integer> children, int x, int y)
    {
        this.modelNode = modelNode;
        this.id = modelNode.id;
        this.parent = parent;
        this.children = children;
        this.tag = modelNode.content;
        this.color = null;
        this.edgeTagColor = null;
        this.x = x;
        this.y = y;
        this.subTreeWidth = 1;
        this.edgeTag = "";
        this.edgeDir = "";
        this.edgeType = "";
        //star at beginning of tag symbolizes dotted edge to parent
        if (tag.startsWith("*"))
        {
            tag = tag.substring(1);
            edgeType = "dotted";
        }
    }
}
