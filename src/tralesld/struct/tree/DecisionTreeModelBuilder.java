package tralesld.struct.tree;

import tralesld.struct.trace.*;

public class DecisionTreeModelBuilder extends TreeModelBuilder<XMLTraceModel>
{
	public TreeModel createTreeModel(XMLTraceModel xmlModel)
    {
		return createTreeModel(xmlModel.root);
    }
	
    public TreeModel createTreeModel(XMLTraceNode xmlModelRoot)
    {
        TreeModel model = new TreeModel();
        TreeModelNode modelRoot = new TreeModelNode(xmlModelRoot.id, xmlModelRoot.getParentLinkCaption());
        model.addNode(modelRoot);
        model.root = modelRoot.id;
        for (XMLTraceNode child : xmlModelRoot.getChildren().values())
        {
        	TreeModelNode modelChild = createTreeModelNode(child, model);
        	model.addNode(modelChild);
        	model.nodes.get(model.root).children.add(modelChild.id);
        	modelChild.parent = modelRoot.id;
        }
        return model;
    }
    
    public TreeModelNode createTreeModelNode(XMLTraceNode xmlModelNode, TreeModel model)
    {
        TreeModelNode modelNode = new TreeModelNode(xmlModelNode.id, xmlModelNode.getParentLinkCaption());
        model.addNode(modelNode);
        for (XMLTraceNode child : xmlModelNode.getChildren().values())
        {
        	TreeModelNode modelChild = createTreeModelNode(child, model);
        	model.addNode(modelChild);
        	model.nodes.get(xmlModelNode.id).children.add(modelChild.id);
        	modelChild.parent = xmlModelNode.id;
        }
        return modelNode;
    }
}
