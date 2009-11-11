package tralesld.visual.bindings;

import gralej.parsers.ParseException;

import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.util.HashMap;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JPanel;

import tralesld.util.Utilities;
import tralesld.util.VisualizationUtility;

public class VariableWatchPanel extends JPanel implements ComponentListener
{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private boolean fresh;

	private VisualizationUtility util;

	private Map<String, String> paintedData = new HashMap<String, String>(0);

	private Map<String, String> currentData;

	public VariableWatchPanel(VisualizationUtility util)
	{
		super();
		this.util = util;
		addComponentListener(this);
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
		if (!Utilities.equal(paintedData, currentData)) {
			boolean needsMessage = true;
			removeAll();
			if (currentData != null)
			{
				for (String key : currentData.keySet())
				{
					JPanel detailFrame = new JPanel();
					try
					{
						JPanel detail = util.visualize(currentData.get(key));
						detailFrame.add(detail);
					} catch (ParseException e)
					{
						detailFrame.add(new JLabel("Parse error: " + e.getMessage()));
					}
					detailFrame.setBorder(BorderFactory.createTitledBorder(key));
					needsMessage = false;
				}
			}

			if (needsMessage)
			{
				add(new JLabel("no variables at this step"));
			}
			repaint();
			paintedData = currentData;
		}
		fresh = true;
	}

}
