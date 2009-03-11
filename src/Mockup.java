import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;
import javax.swing.JPanel;


public class Mockup extends JPanel {
    
    public Mockup() {
	
    }
    
    /**
     * 
     */
    private static final long serialVersionUID = -1328215154091285449L;

    private static void createAndShowGUI() {	
	// Create and set up the window.
	JFrame frame = new JFrame("TraleSLD");
	frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	frame.addWindowListener(new WindowAdapter() {
	    @Override
	    public void windowClosed(WindowEvent we) {
		System.exit(0);
	    }
	});

	// Add content to the window.
	frame.add(new Mockup());

	// Display the window.
	frame.pack();
	frame.setVisible(true);
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
	javax.swing.SwingUtilities.invokeLater(new Runnable() {
	    public void run() {
		createAndShowGUI();
	    }
	});
    }

}
