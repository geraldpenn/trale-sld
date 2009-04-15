package tralesld.gui;

import java.awt.Color;
import java.awt.Graphics2D;
import java.util.HashMap;
import java.util.List;

import tralesld.storage.DataStore;
import tralesld.visual.tree.TreeView;
import tralesld.visual.tree.TreeViewExtension;
import tralesld.visual.tree.TreeViewNode;
import tralesld.visual.tree.TreeViewPanel;

public class ReturnDimensionViewExtension extends TreeViewExtension
{
	DataStore<Integer> forwardLinks;
	HashMap<Integer, Color> nodeColorings;
	
	public ReturnDimensionViewExtension(DataStore<Integer> forwardLinks, HashMap<Integer, Color> nodeColorings)
	{
		this.forwardLinks = forwardLinks;
		this.nodeColorings = nodeColorings;
	}
	
	public void paintOnTreePanel(TreeViewPanel panel, Graphics2D canvas)
	{
		TreeView view = panel.t;
		for (TreeViewNode n : view.treeNodes.values())
		{
			int childID = n.id;
			if (forwardLinks.getData(childID) != null)
			{
				int forwardID = forwardLinks.getData(childID);
				TreeViewNode forward = view.treeNodes.get(forwardID);
				if (forward != null)
				{
					if (n.getEdgeDir().equals("sel"))
					{
						canvas.setColor(nodeColorings.get(childID));
					}
					else
					{
						canvas.setColor(Color.LIGHT_GRAY);
					}
					int c = 80;
					int a = Math.abs(n.x - forward.x) + c;
					int x = Math.abs(n.x - forward.x);
					int y = n.y - forward.y;
					int b = y / 2;
					if (x != 0)
					{
						b = (int) (((a - Math.sqrt(a*a-x*x)) * y * a) / (x * x)); 
					}
					int arcAngle = 180;
					if (x != 0)
					{	
						arcAngle = (int) Math.round((Math.atan((double)(y - b)/((x/(x+50.0))*b)) * 180)/Math.PI);
						if (forward.x - n.x < 0)
						{
							arcAngle = 270 - arcAngle; 
						}
						else
						{
							arcAngle += 90;
						}
					}
					canvas.drawArc(forward.x - a, forward.y, 2 * a, 2 * b, 90 + arcAngle, 360 - arcAngle);
				}
			}
		}
	}
}
