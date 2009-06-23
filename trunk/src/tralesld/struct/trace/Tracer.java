package tralesld.struct.trace;

import java.util.*;

import tralesld.struct.tree.*;
import tralesld.visual.tree.TreeView;

public class Tracer
{
    //these tables contain information on the whole trace with all details
    //partial traces representing the decision finding process will be extracted from here
    int root;
    HashMap<Integer,Integer> parents;
    HashMap<Integer,List<Integer>> children;
    HashMap<Integer,String> desc;
    
    //this part is always displayed, not necessary to store it separately
    public TreeView overviewTraceView;
    
    public Tracer()
    {
        root = -1;
        parents = new HashMap<Integer,Integer>();
        children = new HashMap<Integer,List<Integer>>();
        desc = new HashMap<Integer,String>();
        overviewTraceView = new TreeView();
    }
    
    public int getRoot()
    {
        return root;
    }
    
    public int getParent(int node)
    {
        Integer parent = parents.get(node);
        if (parent == null) return -1;
        return parent;
    }
    
    public List<Integer> getChildren(int node)
    {
        List<Integer> childList = children.get(node);
        if (childList == null)
        {
            childList = new ArrayList<Integer>();
            children.put(node, childList);
        }
        return childList;
    }
    
    public void addChild(int parent, int child)
    {
        List<Integer> childList = getChildren(parent);
        childList.add(child);
        Integer oldParent = parents.get(child);
        if (oldParent != null)
        {
            getChildren(oldParent).remove((Object) child);
        }
        parents.put(child, parent);
    }
    
    public void setParent(int child, int parent)
    {
        addChild(parent,child);
    }
    
    public String getDescription(int node)
    {
        String description = desc.get(node);
        if (description == null) return "?";
        else return description;
    }
    
    public int registerStepByStack(List<Integer> stackList, String shortDescription)
    {
        int currentIndex = -1;
        for (int i = stackList.size() - 1; i >= 0; i++)
        {
            int newIndex = stackList.get(i);
            addChild(currentIndex, newIndex);
            currentIndex = newIndex;
        }
        desc.put(currentIndex, shortDescription);
        return currentIndex;
    }
    
    public int registerStepAsChildOf(int parentID , int stepID, String shortDescription)
    {
        if (parentID == -1) parentID = root;
    	addChild(parentID, stepID);
    	desc.put(stepID, shortDescription);
    	return stepID;
    }
    
    public int getOverviewAncestor(int dtNode)
    {
        while (overviewTraceView.treeNodes.get(dtNode) == null)
        {
            dtNode = getParent(dtNode);
        }
        return dtNode;
    }
}
