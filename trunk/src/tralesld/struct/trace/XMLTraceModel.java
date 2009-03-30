package tralesld.struct.trace;
import java.util.List;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;

import org.w3c.dom.*;

public class XMLTraceModel
{
	Document modelDOM;
	public XMLTraceNode root;
    
    public XMLTraceModel()
    {  	
    	try
    	{
    		modelDOM = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();		
			modelDOM.setXmlVersion("1.0");
			modelDOM.setXmlStandalone(false);
			Element modelRoot = modelDOM.createElement("init");
			modelDOM.appendChild(modelRoot);
    		root = new XMLTraceNode(0,-1,null);
    		root.xmlNode = modelRoot;
    		modelDOM.getDocumentElement().setAttribute("id","-1");
    	}
    	catch (ParserConfigurationException e)
    	{
    		System.err.println("Warning: Failed to initialize XMLTraceModel!");
    	}
    }
    
    public XMLTraceModel(XMLTraceNode root)
    {
    	try
    	{
    		modelDOM = DocumentBuilderFactory.newInstance().newDocumentBuilder().newDocument();		
			modelDOM.setXmlVersion("1.0");
			modelDOM.setXmlStandalone(false);
			modelDOM.appendChild(root.xmlNode);
    		this.root = root;
    	}
    	catch (ParserConfigurationException e)
    	{
    		System.err.println("Warning: Failed to initialize XMLTraceModel!");
    	}
    }
    
    public XMLTraceNode extendModel(List<Integer> address, int content, String shortDescription)
    {
        return root.extendModel(address,content,shortDescription, modelDOM);
    }
    
    public Document getTreeModel()
    {
        return modelDOM;
    }
}
