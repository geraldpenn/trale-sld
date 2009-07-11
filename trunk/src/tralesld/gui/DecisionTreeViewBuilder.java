package tralesld.gui;

import tralesld.TraleSld;
import tralesld.visual.tree.TreeView;

public class DecisionTreeViewBuilder
{
    public TreeView createDecisionTreeView(TraleSld sld)
    {
        TreeView tv = new TreeView();
        tv.setTreeLevelHeight(20);
        tv.setTreeNodesDistance(200);
        int rootID = sld.currentDecisionTreeHead;
        tv.generateNode(rootID, sld.nodeCommands.getData(rootID));
        tv.rootID = rootID; 
        recursivelyCreateDecisionTreeView(rootID, tv, sld);
        return tv;
    }
    
    private void recursivelyCreateDecisionTreeView(int node, TreeView tv, TraleSld sld)
    {
        for (int child : sld.tracer.getChildren(node))
        {
            tv.generateNode(child, sld.nodeCommands.getData(child));
            tv.addChild(node, child);
            //follow through recursion until a node is encountered that belongs to the overview tree
            if (sld.tracer.overviewTraceView.treeNodes.get(child) == null)
            {
                recursivelyCreateDecisionTreeView(child, tv, sld);
            }
        }
    }
}
