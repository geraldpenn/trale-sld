package tralesld.struct.tree;
import java.util.*;

public class TreeModelNode 
{
    public int id;
    public String content;
    public int parent;
    public ArrayList<Integer> children;
    public String parentEdgeLabel;
	
	public TreeModelNode(int id, String content)
	{
		this.id = id;
		this.content = content;
		this.parent = -1;
		this.children = new ArrayList<Integer>();
        this.parentEdgeLabel = "";
	}
}
