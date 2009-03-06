import java.util.*;

public class TreeModelFactory 
{
	//states of the pushdown automaton
	public static int EXPECT_NODE_DESCRIPTION = 0;
	public static int NODE_DESCRIPTION = 1;
	public static int EXPECT_CHILDLIST = 2;
	public static int CHILDLIST = 3;
	public static int COMPLETED_CHILDLIST = 4;
	public static int COMPLETED_NODE = 5;
	public static int PLACEHOLDER = 6;
	
	//parse prolog lists representing trees using a pushdown automaton
	public static TreeModel createTreeModelFromPrologList(String prologList)
	{
		TreeModel model = new TreeModel();
		ArrayList<TreeModelNode> nodeStack = new ArrayList<TreeModelNode>();
		int parsingState = EXPECT_NODE_DESCRIPTION;
		TreeModelNode currentNode = null;
		StringBuffer currentSymbol = new StringBuffer();
		char[] chars = prologList.toCharArray();
		for (int i = 0; i < chars.length; i++)
		{
			switch (chars[i])
			{
				case '[':
					if (parsingState == EXPECT_NODE_DESCRIPTION)
					{
						parsingState = NODE_DESCRIPTION;
					}
					else if (parsingState == EXPECT_CHILDLIST)
					{
						parsingState = CHILDLIST;
					}
					else if (parsingState == CHILDLIST)
					{
						parsingState = NODE_DESCRIPTION;
					}
					else
					{
						System.err.println("Unexpected Token: [ (char " + i + ")");
					}
					break;
				case ']':
					if (parsingState == CHILDLIST)
					{
						currentNode = nodeStack.get(nodeStack.size() - 1);
						TreeModelNode newNode = new TreeModelNode(model.nextFreeID, currentSymbol.toString());
						model.addNode(newNode);
						nodeStack.get(nodeStack.size() - 1).children.add(newNode.id);
						newNode.parent = nodeStack.get(nodeStack.size() - 1).id;
						currentSymbol = new StringBuffer();
						
						parsingState = COMPLETED_CHILDLIST;
					}
					else if (parsingState == COMPLETED_CHILDLIST)
					{
						parsingState = COMPLETED_NODE;
					    currentNode = nodeStack.remove(nodeStack.size() - 1);
					}
					else if (parsingState == COMPLETED_NODE)
					{
						nodeStack.get(nodeStack.size() - 1).children.add(currentNode.id);
						currentNode.parent = nodeStack.get(nodeStack.size() - 1).id;
						parsingState = COMPLETED_CHILDLIST;
					}
					else if (parsingState == PLACEHOLDER)
					{
						TreeModelNode newNode = new TreeModelNode(model.nextFreeID, "?");
						model.addNode(newNode);
						nodeStack.get(nodeStack.size() - 1).children.add(newNode.id);
						newNode.parent = nodeStack.get(nodeStack.size() - 1).id;
						currentSymbol = new StringBuffer();
						
						currentNode = nodeStack.remove(nodeStack.size() - 1);
						
						parsingState = COMPLETED_NODE;
					}
					else
					{	
						System.err.println("Unexpected Token: ] (char " + i + ")");
					}
					break;
				case ',':
					if (parsingState == NODE_DESCRIPTION)
					{
						TreeModelNode newNode = new TreeModelNode(model.nextFreeID, currentSymbol.toString());
						model.addNode(newNode);
						nodeStack.add(newNode);
						currentNode = newNode;
						currentSymbol = new StringBuffer();
						parsingState = EXPECT_CHILDLIST;
					}
					else if (parsingState == COMPLETED_NODE)
					{
						nodeStack.get(nodeStack.size() - 1).children.add(currentNode.id);
						currentNode.parent = nodeStack.get(nodeStack.size() - 1).id;
						parsingState = EXPECT_NODE_DESCRIPTION;
					}
					else
					{
						System.err.println("Unexpected Token: , (char " + i + ")");
					}
					break;
				case '_':
					if (parsingState == EXPECT_CHILDLIST)
					{
						parsingState = PLACEHOLDER;
					}
					else if (parsingState == NODE_DESCRIPTION || parsingState == CHILDLIST)
					{
						currentSymbol.append('_');
					}
					else
					{
						System.err.println("Unexpected Token: _ (char " + i + ")");
					}
					break;
				case ' ':
					break;
				default:
					currentSymbol.append(chars[i]);
			}
		}
		if (nodeStack.size() == 0)
		{
			model.root = currentNode.id;
			//System.err.println("Model size: " + model.nodes.size());
		}
		else
		{
			System.err.println("Parsing went utterly wrong!!!");
		}
		return model;
	}
}
