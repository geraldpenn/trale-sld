import java.util.*;

public class TreeModel 
{
	HashMap<Integer,TreeModelNode> nodes;
	int root;
	int nextFreeID;
	
	boolean usesTerminals;
	boolean usesPreTerminals;
	ArrayList<Integer> terminals;
	
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
}
