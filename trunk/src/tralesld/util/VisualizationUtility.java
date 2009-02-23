package tralesld.util;

import gralej.blocks.BlockPanel;
import gralej.om.IVisitable;
import javax.swing.JPanel;

/**
 *
 * @author ke
 */
public class VisualizationUtility {

    /**
     * 
     * @param fs A typed feature structure in Gralej's internal representation.
     * @return An object representing the visualization of the feature
     *         structure, providing various methods to control rendering,
     *         and a method called <code>getCanvas()</code> to obtain the
     *         actual {@link JPanel}.
     */
    public static BlockPanel visualizeTypedFS(IVisitable fs) {
        return (new BlockPanel(fs));
    }

}