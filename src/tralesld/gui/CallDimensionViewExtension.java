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
					/*int leftX = parent.x;
					int width = Math.abs(n.x - leftX) + 50;
					if (n.x < leftX) leftX = n.x;
					int arcAngle = (int) Math.round((Math.atan((parent.x - n.x) / (n.y - parent.y) * 0.5) * 180)/Math.PI);
					canvas.drawArc(parent.x - width, parent.y, width * 2, n.y - parent.y, 90, 180 - arcAngle);*/
					int c = 50;
					int a = Math.abs(n.x - parent.x) + c;
					int x = Math.abs(n.x - parent.x);
					int y = n.y - parent.y;
					int b = y / 2;
					if (x != 0)
					{
						b = (int) (((a - Math.sqrt(a*a-x*x)) * y * a) / (x * x)); 
					}
					int arcAngle = 180;
					if (x != 0)
					{	
						arcAngle = (int) Math.round((Math.atan((double)(y - b)/((x/(x+50.0))*b)) * 180)/Math.PI);
						if (parent.x - n.x < 0)
						{
							arcAngle = 270 - arcAngle; 
						}
						else
						{
							arcAngle += 90;
						}
					}
					canvas.drawArc(parent.x - a, parent.y, 2 * a, 2 * b, 90, arcAngle);
				}
			}
		}
	}
}
