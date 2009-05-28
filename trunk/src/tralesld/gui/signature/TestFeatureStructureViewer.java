/**
 * 
 */
package tralesld.gui.signature;

import java.awt.BorderLayout;
import java.awt.Color;
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * This class is only meant for testing the class {@link VisualSignature} and
 * its only method VisualSignature.drawSignature(String signature, JPanel panel)
 * 
 * @author fdk
 *
 */
public class TestFeatureStructureViewer extends JFrame {

//	static JPanel panel;
	
//	public FeatureStructureViewer() {
		
//		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
//		this.setTitle("TRALE-sld Test Viewer");
//		
//		//Create panel
//		panel = new JPanel();
//		//Specify layout manager and background color
//		panel.setLayout(new BorderLayout(1,1));
//		panel.setBackground(Color.RED);
//
//		
//		this.getContentPane().add(panel);
		
//	}
	
	public String fileToString(String filename) {

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
			System.out.println("Read error with file " + filename);
			System.exit(-1);
		}	

		return sb.toString();
	}

	
	
	public void run(String sigfilename) {

		String path = "data/";
		
		String sig = fileToString(path + sigfilename);

		
		
		
		JPanel panel;
		
		this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.setTitle("TRALE-sld Test Viewer");
		
		//Create panel
		panel = new JPanel();
		//Specify layout manager and background color
		panel.setLayout(new BorderLayout(1,1));
		panel.setBackground(Color.WHITE);

		this.getContentPane().add(panel);
		


		VisualSignature.drawSignature(sig, panel);
		
		
	}
	
	
	
	/**
	 * @param args
	 */
	public static void main(String[] args) {
		
		TestFeatureStructureViewer fsv = new TestFeatureStructureViewer();

		
		String filename;
//		filename = "signature.txt";
		filename = Tools.inputLine("Signature file: ");

		
		fsv.setSize(300, 300);

		fsv.setVisible(true);

		fsv.run(filename);
		
	}

}
