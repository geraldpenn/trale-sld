package tralesld.visual.tree;

import java.util.*;
import java.awt.*;

import tralesld.struct.tree.*;

public class TreeViewNode 
{
	TreeModelNode modelNode;
    public int id;
    private int parent;
    public ArrayList<Integer> children;
    public String tag;
    public Color color;
    Color edgeTagColor;
    public int x;
    public int y;
    public int subTreeWidth;
	protected String edgeTag;
	protected String edgeDir;

	String edgeType;
    
    public TreeViewNode(TreeModelNode modelNode, int parent, ArrayList<Integer> children, int x, int y)
    {
        this.modelNode = modelNode;
        this.id = modelNode.id;
        this.setParent(parent);
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
    
    public TreeViewNode(int id, int parent, ArrayList<Integer> children, String tag, int x, int y)
    {
        this.modelNode = null;
        this.id = id;
        this.setParent(parent);
        this.children = children;
        this.tag = tag;
        this.color = null;
        this.edgeTagColor = null;
        this.x = x;
        this.y = y;
        this.subTreeWidth = 1;
        this.edgeTag = "";
        this.edgeDir = "";
        this.edgeType = "";
    }
    
    public int getSubTreeWidth()
	{
		return subTreeWidth;
	}
    
    public String getEdgeTag()
	{
		return edgeTag;
	}

	public void setEdgeTag(String edgeTag)
	{
		this.edgeTag = edgeTag;
	}
	
	public String getEdgeDir()
	{
		return edgeDir;
	}

	public void setEdgeDir(String edgeDir)
	{
		this.edgeDir = edgeDir;
	}

	public void setParent(int parent)
	{
		this.parent = parent;
	}

	public int getParent()
	{
		return parent;
	}
}
