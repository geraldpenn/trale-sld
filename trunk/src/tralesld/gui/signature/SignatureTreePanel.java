/**
 * 
 */
package tralesld.gui.signature;

import java.awt.Font;
import java.awt.Graphics;
import java.util.ArrayList;

import javax.swing.JPanel;


/**
 * @author fdk
 *
 */
public class SignatureTreePanel extends JPanel {

	private String signature;
	String fontname = "Verdana";
	int fontstyle = Font.BOLD;
	int fontsize = 10;
	
	int leftmargin = 10;
	int tabwidth = 10;
	float spacing = 1.5f;
	
	public SignatureTreePanel(String signature) {
		this.signature = signature;
	}
	
	protected void paintComponent(Graphics g) { 
		super.paintComponent(g);

		g.setFont(new Font(fontname, fontstyle, fontsize));
		
		
		/*
		 * --------------------------------------------------------------------------------------
		 * Parse the type hierarchy string and build a Tree object
		 * --------------------------------------------------------------------------------------
		 */
		
		ArrayList<String> lines = Tools.getSignatureLines(Tools.stringToLinesArrayList(signature));
// TODO		Tree tree = new Tree(lines);
		
		/*
		 * --------------------------------------------------------------------------------------
		 * Calculate the tree graphics
		 * --------------------------------------------------------------------------------------
		 */

		
		/*
		 * --------------------------------------------------------------------------------------
		 * Draw the tree graphics
		 * --------------------------------------------------------------------------------------
		 */
		
		
		// As long as the "real thing" isn't implemented, we simply draw the indented structure
		String prefix = null;
		if (Tools.startsWithTabOrBlank(lines.get(1))) {
			prefix = Tools.getPrefix(lines.get(1));
		}
		else {
			System.err.println("Subtypes must be indented with at least one tab or blank.");
			System.exit(-1);
			// 
		}
		
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i);

			int leadingprefixes = Tools.countLeadingPrefixes(line, prefix);
			line = Tools.stripLeadingPrefixes(line, prefix);
			if (!Tools.startsWithTabOrBlank(line)) {
				g.drawString(line, leftmargin + (leadingprefixes * tabwidth), (i+1) * (int) (spacing * fontsize));
			}
			else {
				System.err.println("Inconsistent use of tabs and/or blanks for indentation.");
				System.exit(-1);
			}
		}
		
	} 


}
