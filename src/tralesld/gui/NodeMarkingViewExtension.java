package tralesld.gui;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.util.*;

import tralesld.visual.tree.*;

public class NodeMarkingViewExtension extends TreeViewExtension
{
	List<Integer> nodesToMark;
	Color markingColor;
	
	public NodeMarkingViewExtension(List<Integer> nodesToMark, Color markingColor)
	{
		this.nodesToMark = nodesToMark;
		this.markingColor = markingColor;
	}
	
	public void paintOnTreePanel(TreeViewPanel panel, Graphics2D canvas)
	{
		canvas.setColor(markingColor);
		canvas.setStroke(new BasicStroke(2));
		TreeView view = panel.t;
		for (int nID : nodesToMark)
		{
			TreeViewNode node = view.treeNodes.get(nID);
			if (node != null)
			{
				int x = node.x - node.tag.length() * 4;
				int y = node.y - 10;
				int width = node.tag.length() * 8;
                canvas.drawRect(x, y, width, 12);
			}
		}
	}
	
}
