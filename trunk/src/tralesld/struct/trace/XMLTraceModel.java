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
    		modelDOM.getDocumentElement().setAttribute("id","-1");
    		root = new XMLTraceNode(-1,null);	
    		root.xmlNode = modelDOM.getDocumentElement();
    	}
    	catch (ParserConfigurationException e)
    	{
    		System.err.println("Warning: Failed to initialize XMLTraceModel!");
    	}
    }
    
    public int extendModel(List<Integer> address, int content, String shortDescription)
    {
        return root.extendModel(address,content,shortDescription, modelDOM);
    }
    
    public Document getTreeModel()
    {
        return modelDOM;
    }
}
