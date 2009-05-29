/**
 * 
 */
package tralesld.gui.signature;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A class to hold misc methods
 * 
 * @author fdk
 *
 */
public class Tools {

	/**
	 * @param filelines
	 * @return the lines with the signature in it.<br>
	 *         I.e. any lines between the first line consisting of "type_hierarchy" only and the
	 *         next line consisting of "." only (or EOF),
	 *         leaving away all lines for which one of the following is true:<br>
	 *         a) line is empty<br>
	 *         b) line contains blanks and/or tabs only<br>
	 *         c) line starts with a '%'-character<br>
	 *         d) line contains a '%'-character with nothing but blanks and/or tabs in front of it
	 */
	public static ArrayList<String> getSignatureLines(ArrayList<String> filelines) {
		ArrayList<String> siglines = new ArrayList<String>();
		
		/*
		 * Look for the first line with "type_hierarchy" and nothing else in it.
		 * Collect all nonempty lines up to, but not including the first line with "." and nothing else in it.
		 * 
		 * This will return the lines with the first signature.
		 * The TRALE format allows more than one signature in a file, but TRALE also uses only the first one.
		 */
		
		int headLinenum = -1;
		int footLinenum = -1;
		int linenum = 0;
		while (linenum < filelines.size()) {
			if (filelines.get(linenum).equals("type_hierarchy")) {
				headLinenum = linenum;
				break;
			}
			linenum++;
		}
		
		if (headLinenum != -1) {
			// we found a line "type_hierarchy" - let's collect all lines before a line with just one single "."
			
			linenum++;	// look at the next line after "type_hierarchy"
			
			while (linenum < filelines.size()) {
				if (filelines.get(linenum).equals(".")) {
					footLinenum = linenum;
					break;
				}
				else {
					
					if (
							(!isEmptyLine(filelines.get(linenum)))
							&& (!isBlankTabLine(filelines.get(linenum)))
							&& (!isCommentLine(filelines.get(linenum)))
							&& (!isBlankTabCommentLine(filelines.get(linenum)))
					) {
						siglines.add(filelines.get(linenum));
					}
					else {
						/*
						 *  we don't want to collect
						 *  a) empty lines
						 *  b) lines with only tabs/blanks
						 *  c) lines starting with '%'
						 *  d) lines containing '%' and only tabs/blanks in front of it
						 */
					}
					
				}
				linenum++;
			}// end while
			
			/*
			 * Note:
			 * If there is no line ".", anything up to the end of the file is collected
			 */
			
		}
		else {
			/*
			 * there's no line "type_hierarchy" -- return empty ArrayList
			 */
		}
		
		return siglines;
	}
	

	
	public static String getTypeFromSigString(String string) {
		/*
		 * Type and optional AVPairs are seperated by whitespace.
		 * First, we will replace all whitespace by one blank.
		 * Then we will remove an optional '&' at the beginning of the type
		 */
		string = string.replaceAll("[\t ]+", " ");		
		String[] tokens = string.split(" ");
		
		String type;
		if (tokens[0].startsWith("&")) {
			type = tokens[0].substring(1);
		}
		else {
			type = tokens[0];
		}
		
		return type;
	}
	
	/**
	 * @param string
	 * @return list of AVPair objects
	 */
	public static ArrayList<AVPair> getAVPairsFromSigString(String string) {
		
		ArrayList<AVPair> avpairs = new ArrayList<AVPair>();
		
		/*
		 * Type and optional AVPairs are seperated by whitespace.
		 * Replace all whitespace by one blank, then split
		 */
		string = string.replaceAll("[\t ]+", " ");		
		String[] tokens = string.split(" ");

		if (tokens.length > 1) {
			for (int i = 1; i < tokens.length; i++) {
				String token = tokens[i];
//				System.out.println(token);
				String[] avp = token.split(":");
				avpairs.add(new AVPair(avp[0], avp[1]));
			}
		}
		else {
			// No AVPairs
		}
		
		return avpairs;
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


	/**
	 * @param filename
	 * @return content of the file
	 */
	public static String fileToString(String filename) {

		StringBuffer sb = new StringBuffer();
		
		BufferedReader f;
		String line;

		try {
			f = new BufferedReader(new FileReader(filename));
			
			boolean firstline = true; 
			
			while ((line = f.readLine()) != null) {
				if (firstline) {
					sb.append(line);
					firstline = false;
				}
				else {
					sb.append("\n" + line);
				}
			}
			f.close();
			
		} catch (IOException e) {
			System.out.println("Error reading file " + filename);
			System.exit(-1);
		}	

		return sb.toString();
	}

	
	/**
	 * Counts the leading prefixes of the string
	 * @param s
	 * @param prefix
	 * @return number of prefixes
	 */
	public static int countLeadingPrefixes(String s, String prefix) {
		int prefixcount = 0;
		while (s.startsWith(prefix)) {
			prefixcount++;
			s = s.replaceFirst(prefix, "");
		}
		return prefixcount;
	}

	/**
	 * Strips the leading prefixes from the string
	 * @param s
	 * @param prefix
	 * @return string without prefixes
	 */
	public static String stripLeadingPrefixes(String s, String prefix) {
		while (s.startsWith(prefix)) {
			s = s.replaceFirst(prefix, "");
		}
		return s;
	}


	
	/**
	 * Checks if a string starts with a tab or a blank
	 * @param s
	 * @return false if string doesn't start with blank or tab
	 */
	public static boolean startsWithTabOrBlank(String s) {
		boolean startswithtaborblank = false;

		if ((s.startsWith("\t")) || (s.startsWith(" "))) {
			startswithtaborblank = true;
		}
		
		return startswithtaborblank;
	}
	
	/**
	 * @param s
	 * @return any combination of blanks and/or tabs at the beginning of the given string
	 */
	public static String getIndentString(String s) {
		
		String prefix;
		
		Pattern p;
		Matcher m;

		p = Pattern.compile("^([\t ]*).*$");
		m = p.matcher(s);
		m.matches();
		prefix = m.group(1);

		return prefix;
	}
	
	
	/**
	 * Strips leading tabs and blanks from the string 
	 * @param s
	 * @return string without leading tabs and blanks
	 */
	public static String stripLeadingTabsAndBlanks(String s) {
		
		Pattern p;
		Matcher m;

		p = Pattern.compile("^[\t ]*(.*)$");
		m = p.matcher(s);
		m.matches();
		s = m.group(1);

		return s;
	}

	/**
	 * Counts the leading tabs of the string
	 * @param s
	 * @return number of tabs
	 */
	public static int countLeadingTabs(String s) {
		int tabcount = 0;
		while (s.startsWith("\t")) {
			tabcount++;
			s = s.substring(1);
		}
		return tabcount;
	}

	
	
	/**
	 * @param line
	 * @return true if line is completely empty
	 */
	public static boolean isEmptyLine(String line) {
		return line.equals("");
	}
	
	/**
	 * @param line
	 * @return true if line consists of blanks and/or tabs only
	 */
	public static boolean isBlankTabLine(String line) {
		return line.matches("^[\t ]*$");
	}
	
	/**
	 * @param line
	 * @return true if line starts with a '%' character
	 */
	public static boolean isCommentLine(String line) {
		return line.startsWith("%");
	}
	
	/**
	 * @param line
	 * @return true if a line contains a '%' character and nothing but blanks and/or tabs in front of it
	 */
	public static boolean isBlankTabCommentLine(String line) {
		return line.matches("^[\t ]*%.*$");
	}
	
	
	
	public static String inputLine(String prompt) {
		String line = null;
		
		InputStreamReader isr = new InputStreamReader(System.in);
		BufferedReader din = new BufferedReader(isr);
		System.out.print(prompt);
		try {
			line = din.readLine();
		} catch (IOException e) {
			e.printStackTrace();
			die();
		}
		
		return line;
	}
	public static String inputLine() {
		return inputLine("");
	}
	
	public static void die(String s) {
		System.out.println(s);
		System.exit(-1);
	}
	public static void die() {
		die("");
	}
	
	

	/**
	 * Creates a SigDirectedGraph object from a file with a graph in the following format:
	 * 
	 * Every line of the file is either a node definition line or an edge definition line.
	 * 
	 * Node definition lines consist of one string without any whitespace characters.
	 * Even though the node gets a unique ID, the string has to be unique, too.
	 * 
	 * Edge definition lines consist of two strings without any whitespace characters,
	 * separated by one TAB character ('\t').  Both strings have to be defined node labels
	 * in one of the node definition lines.
	 * 
	 * Note: Edge definition lines need not be unique.
	 * 
	 * 
	 * @param filename
	 * @return
	 */
	public static SigDirectedGraph constructDirectedGraphFromFile(String filename) {
		SigDirectedGraph mdg = new SigDirectedGraph();
		
		/*
		 * get file and parse it
		 */
		
		ArrayList<String> lines = fileToStringArrayList(filename);

		/*
		 * first create all nodes, because we need them before we create the edges
		 */
		int id = 0;
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i);
			
			if (!line.contains("\t")) {
				// node
				SigGraphNode node = new SigGraphNode();
				node.setType(line);
				node.setId(id++);
				mdg.addNode(node);
			}
		}
		
		for (int i = 0; i < lines.size(); i++) {
			String line = lines.get(i);
			
			if (line.contains("\t")) {
				// edge
				/*
				 * find the appropriate nodes and construct the edge
				 */
				
				String[] tokens = line.split("\t");
				
				String typeSource = tokens[0];
				String typeTarget = tokens[1];
				
				ArrayList<SigGraphNode> sourcenodes = mdg.getNodesWithType(typeSource);
				if (sourcenodes.size() != 1){
					die("Source not found or not unique.");
				}
				
				ArrayList<SigGraphNode> targetnodes = mdg.getNodesWithType(typeTarget);
				if (targetnodes.size() != 1){
					die("Target not found or not unique.");
				}
				
				SigGraphEdge edge = new SigGraphEdge(sourcenodes.get(0), targetnodes.get(0));
				mdg.addEdge(edge);
				
				
				
			}
		}
		
		return mdg;
	}
	
	
	public static ArrayList<String> fileToStringArrayList(String filename) {

		ArrayList<String> lines = new ArrayList<String>();

		BufferedReader f;
		String line;

		try {
			f = new BufferedReader(new FileReader(filename));
			
			while ((line = f.readLine()) != null) {
				lines.add(line);
			}
			f.close();
			
		} catch (IOException e) {
			Tools.die("Read error with file " + filename);
		}	

		return lines;
	}
	
	
	/**
	 * Given integers a and b, find integer c in the middle between them.
	 * if (a+b)/2 is not an integral value, pick the closest one less than
	 * that value.
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static int intmid(int a, int b) {
		int c = 0;
		
		/*
		 * given integers a and b, find integer c in the middle between them.
		 * if (a+b)/2 is not an integral value, pick the closest one less than
		 * that value.
		 * 
		 * examples:
		 * a=2, b=2 --> c=2 (trivial case)
		 * a=2, b=3 --> c=2
		 * a=2, b=4 --> c=3
		 * a=2, b=5 --> c=3
		 */

		c = (a + b) / 2;
		return c;
	}
	
	
	public static void main(String[] args) {

		
		int a = 2;
		int b = 6;
		System.out.println("a=" + a + " b=" + b + " --> c=" + intmid(a, b));
		

//		String s = null;
//		s = "typ  \t ";
//		s = "typ  \t f:v\t        f2:v2";
//		s = "typ f:v";
//		s = "typ          ";
//		s = "typ";
//		s = "";

//		System.out.println(">>>" + getTypeFromSigString("typ  \t ") + "<<<");
//		System.out.println(">>>" + getTypeFromSigString("typ  \t f:v\t        f2:v2") + "<<<");
//		System.out.println(">>>" + getTypeFromSigString("typ          ") + "<<<");
//		System.out.println(">>>" + getTypeFromSigString("typ") + "<<<");
		
//		System.out.println(">>>" + getTypeFromSigString(s) + "<<<");
//		
//		ArrayList<AVPair> avpairs = getAVPairsFromSigString(s);
//		for (int i = 0; i < avpairs.size(); i++) {
//			AVPair avpair = avpairs.get(i);
//			
//			System.out.println(avpair.toString());
//			
//		}
		
		
		
		
		
	}
	
}
