package tralesld.struct.trace;
import java.util.*;

import org.w3c.dom.*;

public class XMLTraceNode
{
    static int number = 0;
    static Map<Integer,XMLTraceNode> nodes = new HashMap<Integer,XMLTraceNode>();
    
    public int id;
    private int content;
    private HashMap<Integer,XMLTraceNode> children;
    private XMLTraceNode parent;
    
    public XMLTraceNode getParent() 
    {
		return parent;
	}

	private String parentLinkCaption;
    
    Element xmlNode;
    
    public XMLTraceNode(int id, int content, XMLTraceNode parent)
    {
        this.id = id;
        number++;
        this.content = content;
        this.parent = parent;
        parentLinkCaption = "";
        children = new HashMap<Integer,XMLTraceNode>();  
        nodes.put(id, this);
    }
    
    public XMLTraceNode(int id, int content, XMLTraceNode parent, Document dom)
    {
        this.id = id;
        number++;
        this.content = content;
        this.parent = parent;
        this.parent.children.put(id, this);
        parentLinkCaption = "";
        children = new HashMap<Integer,XMLTraceNode>();
        nodes.put(id, this);
        
        xmlNode = dom.createElement("node");
        xmlNode.setAttribute("content", content + "");
        xmlNode.setAttribute("parentLinkCaption", parentLinkCaption);
        parent.xmlNode.appendChild(xmlNode);     
    }
    
    public XMLTraceNode extendModel(List<Integer> address, int content, String shortDescription, Document dom)
    {      
        if (address.size() > 0)
        {
            int addressTail = address.remove(address.size() - 1);
            if (children.get(addressTail) == null)
            {
                children.put(addressTail, new XMLTraceNode(addressTail,-1,this, dom));
            }         
            return children.get(addressTail).extendModel(address,content,shortDescription, dom);
        }
        else
        {
            this.setContent(content);
            this.parentLinkCaption = shortDescription;
            this.xmlNode.setAttribute("content", content + "");
            this.xmlNode.setAttribute("parentLinkCaption", parentLinkCaption);
            return this;
        }
    }

	public String getParentLinkCaption() 
	{
		return id + "";
		//return parentLinkCaption;
	}

	public void setParentLinkCaption(String parentLinkCaption) 
	{
		this.parentLinkCaption = parentLinkCaption;
		this.xmlNode.setAttribute("parentLinkCaption", parentLinkCaption);
	}

	public void setContent(int content) 
	{
		this.content = content;
		this.xmlNode.setAttribute("content", content + "");
	}

	public int getContent() 
	{
		return content;
	}
	
	public Map<Integer, XMLTraceNode> getChildren()
	{
		return children;
	}
  
}
