import java.util.*;

public class TreeModel 
{
	HashMap<Integer,TreeModelNode> nodes;
	int root;
	int nextFreeID;
	
	public TreeModel()
	{
		nodes = new HashMap<Integer,TreeModelNode>();
		root = -1;
		nextFreeID = 0;
	}
	
	public void addNode(TreeModelNode node)
	{
		nodes.put(node.id,node);
		nextFreeID++;
	}
}
