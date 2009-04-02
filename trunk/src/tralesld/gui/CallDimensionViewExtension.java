package tralesld.gui;

import java.awt.*;

import java.util.List;
import tralesld.storage.*;
import tralesld.visual.tree.*;

public class CallDimensionViewExtension extends TreeViewExtension 
{
	DataStore<Integer> parentLinks;
	List<Integer> nodesToMark;
	
	public CallDimensionViewExtension(DataStore<Integer> parentLinks, List<Integer> nodesToMark)
	{
		this.parentLinks = parentLinks;
		this.nodesToMark = nodesToMark;
	}
	
	public void paintOnTreePanel(TreeViewPanel panel, Graphics2D canvas)
	{
		nodesToMark.clear();
		TreeView view = panel.t;
		for (TreeViewNode n : view.treeNodes.values())
		{
			int childID = n.id;
			if (parentLinks.getData(childID) != null)
			{
				int parentID = parentLinks.getData(childID);
				TreeViewNode parent = view.treeNodes.get(parentID);
				if (parent != null)
				{
					if (parent.color == Color.YELLOW)
					{
						nodesToMark.add(childID);
						canvas.setColor(Color.YELLOW);
					}
					else
					{
						canvas.setColor(Color.LIGHT_GRAY);
					}
					int leftX = parent.x;
					int width = Math.abs(n.x - leftX) + 50;
					if (n.x < leftX) leftX = n.x;
					int arcAngle = (int) Math.round((Math.atan((parent.x - n.x) / (n.y - parent.y) * 0.5) * 180)/Math.PI);
					canvas.drawArc(leftX - 50, parent.y, width * 2, n.y - parent.y, 90, 180 - arcAngle);
				}
			}
		}
	}
}
