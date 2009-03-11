package tralesld;

import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JFrame;

import tralesld.gui.control.ControlPanel;

public class Main {

    private static void createAndShowGUI() {
	// Create and connect view and controller
	final Controller controller = new Controller();
	ControlPanel controlPanel = new ControlPanel(controller);
	
	// Start the server:
	controller.startServer();
	
	// Create and set up the window.
	JFrame frame = new JFrame("TraleSLD");
	frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
	frame.addWindowListener(new WindowAdapter() {
	    @Override
	    public void windowClosed(WindowEvent we) {
		controller.exit();
		System.exit(0);
	    }
	});

	// Add content to the window.
	frame.add(controlPanel);

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
