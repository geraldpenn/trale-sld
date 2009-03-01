import java.util.*;

public class TreeView 
{
	//internal information
    ArrayList<ArrayList<Integer>> nodeLevels;
    HashMap<Integer,TreeViewNode> treeNodes;
    int rootID;
    
    //view options
    int treeNodesDistance = 50;
    int treeLevelHeight = 25;
    int totalTreeWidth;
    int totalTreeHeight;
    
    public TreeView()
    {
    	rootID = -1;
        totalTreeWidth = 0;
        totalTreeHeight = 0;
        nodeLevels = new ArrayList<ArrayList<Integer>>();
        treeNodes = new HashMap<Integer,TreeViewNode>();
    }
    
    public TreeView(TreeModel model)
    {
    	rootID = -1;
        totalTreeWidth = 0;
        totalTreeHeight = 0;
        createTreeStructure(model);
        createNodeLayers();
        calculateCoordinates();   
    }
    
    private void createTreeStructure(TreeModel model)
    {
		nodeLevels = new ArrayList<ArrayList<Integer>>();
        treeNodes = new HashMap<Integer,TreeViewNode>();
    	if (model.root != -1)
    	{
    		TreeModelNode rootModel = model.nodes.get(model.root);
    		TreeViewNode root = new TreeViewNode(rootModel,-1,new ArrayList<Integer>(),100, 50);
    		treeNodes.put(root.id,root);
    		rootID = root.id;
    		createSubtreeStructure(rootModel,root, model); 
    	}
    }
    
    private void createSubtreeStructure(TreeModelNode modelNode, TreeViewNode viewNode, TreeModel model)
    {
        for (int i = 0; i < modelNode.children.size(); i++)
        {
            TreeModelNode currentChild = model.nodes.get(modelNode.children.get(i));
            TreeViewNode childNode = new TreeViewNode(currentChild,viewNode.id,new ArrayList<Integer>(),viewNode.x, viewNode.y + treeLevelHeight);
            viewNode.x += treeNodesDistance;
            treeNodes.put(childNode.id,childNode);
            viewNode.children.add(childNode.id);
            createSubtreeStructure(currentChild,childNode,model);
        }
    }
    
    public void createNodeLayers()
    {
      int level = 0;
      TreeViewNode root = treeNodes.get(rootID);
      nodeLevels = new ArrayList<ArrayList<Integer>>();
      ArrayList<Integer> rootLevel = new ArrayList<Integer>();
      rootLevel.add(rootID);
      nodeLevels.add(level,rootLevel);
      level++;
      ArrayList<Integer> children = root.children;
      while(true)
      {
        ArrayList<Integer> grandchildren = new ArrayList<Integer>();
        for (int i = 0; i < children.size(); i++)
        {
            for (int j = 0; j < treeNodes.get(children.get(i)).children.size(); j++)
            {
                grandchildren.add(treeNodes.get(children.get(i)).children.get(j));
            }
        }
        nodeLevels.add(level,children);
        children = grandchildren;
        level++;
        if (grandchildren.size() == 0)
        {
            break;
        }
      }
    }
    
    public void calculateCoordinates()
    {
        createNodeLayers();
        totalTreeWidth = 0;
        totalTreeHeight = (nodeLevels.size() + 1) * treeLevelHeight;
        //calculate (maximum) subtree width for each node bottom-up
        for(int i = nodeLevels.size() - 1; i >= 0; i--)
        {
            ArrayList<Integer> nodes = nodeLevels.get(i); 
            for (int j = 0; j < nodes.size(); j++)
            {
                TreeViewNode node = treeNodes.get(nodes.get(j));
                if (node.children.size() > 0)
                {
                	node.subTreeWidth = collectWidths(node.children);   
                }
                else
                {
                	node.subTreeWidth = 1;
                }
            }
        }
        treeNodes.get(rootID).x = treeNodes.get(rootID).subTreeWidth * treeNodesDistance/2;
        //no edges may cross, no nodes overlap
        for(int i = 0; i < nodeLevels.size(); i++)
        {
            ArrayList<Integer> nodes = nodeLevels.get(i);  
            int xOffset = 100;
            int parent = -1;
            for (int j = 0; j < nodes.size(); j++)
            {
                int subtreeWidth = treeNodes.get(nodes.get(j)).subTreeWidth * treeNodesDistance;
                xOffset += subtreeWidth;
                if (i > 0 && treeNodes.get(nodes.get(j)).parent != parent)
                {
                    parent = treeNodes.get(nodes.get(j)).parent;
                    xOffset = (int)(treeNodes.get(parent).x +  treeNodes.get(parent).subTreeWidth * ((double)(treeNodes.get(nodes.get(j)).subTreeWidth)/treeNodes.get(parent).subTreeWidth - 0.5) * treeNodesDistance);
                }
                if (i > 0)
                {
                    treeNodes.get(nodes.get(j)).x =  xOffset - subtreeWidth/2;
                }
            }
            if (nodes.size() > 0 && treeNodes.get(nodes.get(nodes.size() - 1)).x + treeNodesDistance > totalTreeWidth)
            {
                totalTreeWidth = treeNodes.get(nodes.get(nodes.size() - 1)).x + treeNodesDistance;
            }
        }
    }
    
    private int collectWidths(ArrayList<Integer> children)
    {
        int sum = 0;
        for (int i = 0; i < children.size(); i++)
        {
            sum += treeNodes.get(children.get(i)).subTreeWidth;
        }
        return sum;
    }
}
