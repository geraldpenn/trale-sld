package tralesld.util;

import gralej.blocks.BlockPanel;
import gralej.om.IVisitable;
import javax.swing.JPanel;

/**
 *
 * @author ke
 */
public class VisualizationUtility {

    public static JPanel visualizeTypedAVM(IVisitable avm) {
        return (new BlockPanel(avm)).getCanvas();
    }

}
