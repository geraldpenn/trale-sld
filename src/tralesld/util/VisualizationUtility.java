package tralesld.util;

import gralej.Config;
import gralej.controller.StreamInfo;
import gralej.parsers.GraleParserFactory;
import gralej.parsers.IGraleParser;
import gralej.parsers.ParseException;
import gralej.parsers.UnsupportedProtocolException;

import java.io.ByteArrayInputStream;

import javax.swing.JPanel;

/**
 * 
 * @author ke
 */
public class VisualizationUtility {

	private IGraleParser parser;

	public VisualizationUtility() {
		try {
			parser = GraleParserFactory.createParser(StreamInfo.GRISU);
		} catch (UnsupportedProtocolException e) {
			throw new RuntimeException("could not create Grisu format parser",
					e);
		}
		
		Config config = gralej.Config.currentConfig();
	    config.set("behavior.selectOnClick", true);
	    config.set("block.panel.different.background.color", "0xffffaa");
	    config.set("behavior.nodeContentInitiallyVisible", true);
	}

	/**
	 * 
	 * @param grisuMessage
	 *            A typed feature structure or tree in Grisu format.
	 * @return An object representing the visualization of the feature
	 *         structure, providing various methods to control rendering, and a
	 *         method called <code>getCanvas()</code> to obtain the actual
	 *         {@link JPanel}.
	 */
	public JPanel visualize(String grisuMessage) throws ParseException {
		return parser.parseAll(
				new ByteArrayInputStream(grisuMessage.getBytes()),
				StreamInfo.GRISU).get(0).createView().getCanvas();
	}

}