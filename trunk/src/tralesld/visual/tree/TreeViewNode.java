package tralesld.visual.tree;

import java.util.*;
import java.awt.*;

import tralesld.struct.tree.*;

public class TreeViewNode 
{
	TreeModelNode modelNode;
    public int id;
    int parent;
    public ArrayList<Integer> children;
    String tag;
    public Color color;
    Color edgeTagColor;
    public int x;
    public int y;
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
