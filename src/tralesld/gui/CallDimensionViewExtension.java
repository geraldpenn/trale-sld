package tralesld.gui;

import java.awt.*;

import tralesld.storage.*;
import tralesld.visual.tree.*;

public class CallDimensionViewExtension extends TreeViewExtension 
{
	DataStore<Integer> parentLinks;
	
	public CallDimensionViewExtension(DataStore<Integer> parentLinks)
	{
		this.parentLinks = parentLinks;
	}
	
	public void paintOnTreePanel(TreeViewPanel panel, Graphics2D canvas)
	{
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
						canvas.setColor(Color.BLACK);
					}
					else
					{
						canvas.setColor(Color.LIGHT_GRAY);
					}
					int leftX = parent.x;
					int width = Math.abs(n.x - leftX) + 50;
					if (n.x < leftX) leftX = n.x;
					System.err.println(Math.atan((parent.x - n.x) / ((n.y - parent.y) * 0.5)));
					int arcAngle = (int) Math.round((Math.atan((parent.x - n.x) / (n.y - parent.y) * 0.5) * 180)/Math.PI);
					canvas.drawArc(leftX - 50, parent.y, width * 2, n.y - parent.y, 90, 180 - arcAngle);
				}
			}
		}
	}
}
