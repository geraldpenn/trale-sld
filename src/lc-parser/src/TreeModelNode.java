import java.util.*;

public class TreeModelNode 
{
	int id;
	String content;
	int parent;
	ArrayList<Integer> children;
	
	public TreeModelNode(int id, String content)
	{
		this.id = id;
		this.content = content;
		this.parent = -1;
		this.children = new ArrayList<Integer>();
	}
}
