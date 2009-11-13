package tralesld.visual.bindings;

import gralej.parsers.ParseException;

import java.awt.Container;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.HashMap;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;

import tralesld.util.Utilities;
import tralesld.util.VisualizationUtility;

public class VariableWatchPanel extends JPanel implements ComponentListener
{

	// TODO scroll pane
	// TODO autoresize

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private boolean fresh;

	private VisualizationUtility util;

	private Map<String, String> paintedData = new HashMap<String, String>(0);

	private Map<String, String> currentData;
	
	private JPanel innerPanel;

	public VariableWatchPanel()
	{
		super();
		util = VisualizationUtility.getDefault();
		addComponentListener(this);
		innerPanel = new JPanel();
		setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
		add(new JScrollPane(innerPanel));
		refresh();
	}

	public void update(Map<String, String> data)
	{
		currentData = data;
		if (isShowing())
		{
			refresh();
		} else
		{
			fresh = false;
		}
	}

	@Override
	public void componentHidden(ComponentEvent e)
	{
		// do nothing
	}

	@Override
	public void componentMoved(ComponentEvent e)
	{
		// do nothing
	}

	@Override
	public void componentResized(ComponentEvent e)
	{
		// do nothing
	}

	@Override
	public void componentShown(ComponentEvent e)
	{
		if (!fresh)
		{
			refresh();
		}
	}

	private void refresh()
	{
		if (!Utilities.equal(paintedData, currentData))
		{
			boolean needsMessage = true;
			innerPanel.removeAll();
			if (currentData != null)
			{
				System.out.println("adding vars");
				for (String key : currentData.keySet())
				{
					System.out.println("adding " + key);
					JPanel detailFrame = new JPanel();
					try
					{
						JPanel detail = util.visualize(currentData.get(key));
						detailFrame.add(detail);
						System.out.println("R++++ " + key);
					} catch (ParseException e)
					{
						detailFrame.add(new JLabel("Parse error: " + e.getMessage()));
					}
					detailFrame.setBorder(BorderFactory.createTitledBorder(key));
					innerPanel.add(detailFrame);
					needsMessage = false;
					System.out.println("added " + key);
				}
				System.out.println("added vars");
			}

			if (needsMessage)
			{
				innerPanel.add(new JLabel("no variables at this step"));
			}
			Container parent = getParent();
			if (parent instanceof JTabbedPane)
			{
				parent.repaint();
			} else
			{
				repaint();
			}
			paintedData = currentData;
		}
		fresh = true;
	}

}
