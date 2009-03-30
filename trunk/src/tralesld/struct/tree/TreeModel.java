package tralesld.struct.tree;
import java.util.*;

public class TreeModel 
{
	public HashMap<Integer,TreeModelNode> nodes;
    public int root;
    public int nextFreeID;
	
    public boolean usesTerminals;
    public boolean usesPreTerminals;
    public ArrayList<Integer> terminals;
	
	public TreeModel()
	{
		nodes = new HashMap<Integer,TreeModelNode>();
		root = -1;
		nextFreeID = 0;
		usesTerminals = false;
		usesPreTerminals = false;
		terminals = null;
	}
	
	public void useTerminals(boolean usesTerminals)
	{
		this.usesTerminals = usesTerminals;
		if (usesTerminals)
		{
			terminals = new ArrayList<Integer>();
		}
		else
		{
			terminals = null;
		}
	}
	
	public void usePreTerminals(boolean usesPreTerminals)
	{
		this.usesPreTerminals = usesPreTerminals;
		if (usesPreTerminals)
		{
			useTerminals(true);
		}
	}
	
	public void addTerminal(int termID)
	{
		int i = 0;
		while(i < terminals.size() && termID > terminals.get(i))
		{
			i++;
		}
		terminals.add(i, termID);
	}
	
	public void addNode(TreeModelNode node)
	{
		nodes.put(node.id,node);
		nextFreeID++;
	}
	
	public String toString()
	{
		return nodeToString(root,0);
	}
	
	private String nodeToString(int id, int offset)
	{
		TreeModelNode node = nodes.get(id);
		String root = "";
		for (int j = 0; j < offset; j++)
		{
			root += "  ";
		}
		root += node.content + "\n";
		for (int i : node.children)
		{
			root += nodeToString(i ,offset + 1);
		}
		return root;
	}
}
