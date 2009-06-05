package tralesld.gui;

import java.awt.*;

import java.util.List;
import tralesld.storage.*;
import tralesld.visual.tree.*;

public class CallDimensionViewExtension extends TreeViewExtension 
{
	DataStore<Integer> parentLinks;
	DataStore<Integer> recursionDepths;
	List<Integer> nodesToMark;
	
	public CallDimensionViewExtension(DataStore<Integer> parentLinks, DataStore<Integer> recursionDepths, List<Integer> nodesToMark)
	{
		this.parentLinks = parentLinks;
		this.recursionDepths = recursionDepths;
		this.nodesToMark = nodesToMark;
	}
	
	public void paintOnTreePanel(TreeViewPanel panel, Graphics2D canvas)
	{
		nodesToMark.clear();
		TreeView view = panel.t;
		canvas.setStroke(new BasicStroke(1));
		canvas.setColor(Color.LIGHT_GRAY);
		for (int childID = 0, sucChildID = 0; sucChildID < view.treeNodes.size(); childID++)
		{	
			TreeViewNode n = view.treeNodes.get(childID);
			if (n == null) continue;
			sucChildID++;
			//visualize call stack structure by indentation
			panel.t.getIndentations().put(childID,recursionDepths.getData(childID) * 10);
			//edge dir field misused to transport selection information
			if (view.treeNodes.get(childID).getEdgeDir().equals("sel"))
			{
				nodesToMark.add(0, childID);
			}
			//draw edgy line to parent node
			if (parentLinks.getData(childID) != null && !view.getInvisibleNodes().contains(childID))
			{
				int parentID = parentLinks.getData(childID);
				drawEdgyLine(panel.t,childID,parentID,canvas);	
			}
		}
		if (nodesToMark.size() > 0)
		{
			canvas.setStroke(new BasicStroke(2));
			canvas.setColor(Color.BLACK);
			int selectedID = nodesToMark.get(0);
			//draw edgy line to parent node
			if (parentLinks.getData(selectedID) != null && !view.getInvisibleNodes().contains(selectedID))
			{
				int parentID = parentLinks.getData(selectedID);
				drawEdgyLine(panel.t,selectedID,parentID,canvas);			
			}
		}
	}
	
	private void drawEdgyLine(TreeView view, int childID, int parentID, Graphics2D canvas)
	{
		TreeViewNode n = view.treeNodes.get(childID);
		TreeViewNode parent = view.treeNodes.get(parentID);
		if (parent != null)
		{
			int x1 = parent.x + view.getIndent(parentID);
			int x2 = n.x + view.getIndent(childID);
			int y1 = parent.y;
			int y2 = n.y;
			int x3 = x1;
			if (x2 <= x1) x3 = x2;
			canvas.drawLine(x3 - 10, y1, x1, y1);
			canvas.drawLine(x3 - 10, y1, x3 - 10, y2 - 5);
			canvas.drawLine(x3 - 10, y2 - 5, x2, y2 - 5);
		}
	}
	
	//old version: ovals partly marked in yellow
	/*public void paintOnTreePanel(TreeViewPanel panel, Graphics2D canvas)
	{
		nodesToMark.clear();
		TreeView view = panel.t;
		for (TreeViewNode n : view.treeNodes.values())
		{
			int childID = n.id;
			//edge dir field misused to transport selection information
			if (n.getEdgeDir().equals("sel"))
			{
				nodesToMark.add(0, childID);
			}
			if (parentLinks.getData(childID) != null)
			{
				int parentID = parentLinks.getData(childID);
				TreeViewNode parent = view.treeNodes.get(parentID);
				if (parent != null)
				{
					if (parent.getEdgeDir().equals("sel"))
					{
						nodesToMark.add(childID);
						canvas.setColor(Color.YELLOW);
					}
					else
					{
						canvas.setColor(Color.LIGHT_GRAY);
					}
					int c = 80;
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
	}*/
}
