package tralesld.gui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.util.*;

import tralesld.visual.tree.*;

public class NodeMarkingViewExtension extends TreeViewExtension
{
	//first node will be interpreted as main marked node (thicker marking)
	List<Integer> nodesToMark;
	Color markingColor;
	
	public NodeMarkingViewExtension(List<Integer> nodesToMark, Color markingColor)
	{
		this.nodesToMark = nodesToMark;
		this.markingColor = markingColor;
	}
	
	public void paintOnTreePanel(TreeViewPanel panel, Graphics2D canvas)
	{
		
		if (nodesToMark.size() > 0)
		{
			TreeView view = panel.t;
			FontMetrics fm = canvas.getFontMetrics();
	    	
			Integer selected = nodesToMark.remove(0);
			canvas.setColor(markingColor);
			canvas.setStroke(new BasicStroke(4));
			TreeViewNode node = view.treeNodes.get(selected);
			if (node != null)
			{	
		    	int width = fm.stringWidth(node.tag);
				int x = node.x - width/2 - 4;
				if (panel.getNodePositioning() == TreeViewPanel.LEFT_ALIGNMENT)
				{
					x = node.x - 4;
				}
				x += panel.t.getIndent(selected);
				int y = node.y - 12;
                canvas.drawRect(x, y, width + 8, 16);
			}
			
			canvas.setStroke(new BasicStroke(2));		
			for (int nID : nodesToMark)
			{
				node = view.treeNodes.get(nID);
				if (node != null)
				{
			    	int width = fm.stringWidth(node.tag);
					int x = node.x - width/2 - 2;
					int y = node.y - 10;
	                canvas.drawRect(x, y, width + 4, 12);
				}
			}
		}
	}
	
}
