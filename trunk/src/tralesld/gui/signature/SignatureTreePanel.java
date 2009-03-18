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
	int fontsize = 18;
	
	int leftmargin = 20;
	int tabwidth = 50;
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
		
		ArrayList<String> lines = stringToLinesArrayList(signature);
//		Tree tree = new Tree(lines);
		
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
		
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i);

			int leadingtabs = countLeadingTabs(line);
			line = stripLeadingTabs(line);
			
			g.drawString(line, leftmargin + (leadingtabs * tabwidth), (i+1) * (int) (spacing * fontsize));
		}
		
	} 


	
	
	/**
	 * Counts the leading tabs of the string
	 * @param s
	 * @return number of tabs
	 */
	private int countLeadingTabs(String s) {
		int tabcount = 0;
		while (s.startsWith("\t")) {
			tabcount++;
			s = s.substring(1);
		}
		return tabcount;
	}

	/**
	 * Strips the leading tabs from the string 
	 * @param s
	 * @return string without leading tabs
	 */
	private String stripLeadingTabs(String s) {
		while (s.startsWith("\t")) {
			s = s.substring(1);
		}
		return s;
	}

	
	/**
	 * Splits the given string into lines.
	 * 
	 * @param string
	 * @return ArrayList of strings/lines
	 */
	public static ArrayList<String> stringToLinesArrayList(String string) {
		ArrayList<String> strings = new ArrayList<String>();
		
		String[] tokens = string.split("\n");
		for (int i = 0; i < tokens.length; i++) {
			strings.add(tokens[i]);
		}
		
		return strings;
	}

}
