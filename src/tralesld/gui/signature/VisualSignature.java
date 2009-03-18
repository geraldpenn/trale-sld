/**
 * 
 */
package tralesld.gui.signature;

import javax.swing.JPanel;


/**
 * @author fdk
 *
 */
public class VisualSignature {	
	
	/**
	 * @param signature A type hierarchy in TRALE's signature specification file format
	 *        specified at <a href="http://www.ale.cs.toronto.edu/docs/man/ale_trale_man/ale_trale_man-node25.html">http://www.ale.cs.toronto.edu/docs/man/ale_trale_man/ale_trale_man-node25.html</a>
	 *        TODO This not completely implemented yet!
	 *        We expect the nonempty lines between the marker lines ("type_hierarchy" and "."),
	 *        containing the type hierarchy.  We also expect the feature-value pairs all in one line, separated by 1 blank.
	 * @param panel A JPanel used for the graphical representation of the type hierarchy
	 */
	public static void drawSignature(String signature, JPanel panel) {
		panel.add(new SignatureTreePanel(signature));
	}
}
