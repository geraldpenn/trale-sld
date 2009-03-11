package tralesld.gui.control;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JPanel;

import tralesld.Controller;

public class ControlPanel extends JPanel implements ActionListener, KeyListener {

    /**
     * 
     */
    private static final long serialVersionUID = 4175076355147493777L;

    private Controller controller;

    private JButton creepButton;
    
    private JButton skipButton;
    
    private JButton retryButton;
    
    private JButton queryButton;
    
    private JButton abortButton;
    
    private JButton failButton;
    
    private JButton leapButton;
    
    private JButton addBreakpointButton;
    
    private JButton removeBreakpointButton;

    public ControlPanel(Controller controller) {
	super();
	this.controller = controller;

	setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
	
	creepButton = new JButton("creep");
	skipButton = new JButton("skip");
	retryButton = new JButton("retry");
	queryButton = new JButton("query");
	abortButton = new JButton("abort");
	failButton = new JButton("fail");
	leapButton = new JButton("leap");
	addBreakpointButton = new JButton("+");
	removeBreakpointButton = new JButton("-");
	
	creepButton.addActionListener(this);
	skipButton.addActionListener(this);
	retryButton.addActionListener(this);
	queryButton.addActionListener(this);
	abortButton.addActionListener(this);
	failButton.addActionListener(this);
	leapButton.addActionListener(this);
	addBreakpointButton.addActionListener(this);
	removeBreakpointButton.addActionListener(this);
	
	add(creepButton);
	add(skipButton);
	add(retryButton);
	add(queryButton);
	add(abortButton);
	add(failButton);
	add(leapButton);
	add(addBreakpointButton);
	add(removeBreakpointButton);
    }
    
    @Override
    public void actionPerformed(ActionEvent ae) {
	Object source = ae.getSource();
	
	if (source == creepButton) {
	    controller.creep();
	} else if (source == skipButton) {
	    controller.skip();
	} else if (source == retryButton) {
	    controller.retry();
	} else if (source == queryButton) {
	    // TODO
	} else if (source == abortButton) {
	    controller.abort();
	} else if (source == failButton) {
	    controller.fail();
	} else if (source == leapButton) {
	    controller.leap();
	} else if (source == addBreakpointButton) {
	    // TODO
	} else if (source == removeBreakpointButton) {
	    // TODO
	}
    }

    @Override
    public void keyPressed(KeyEvent ke) {
	// TODO Auto-generated method stub

    }

    @Override
    public void keyReleased(KeyEvent ke) {
	// TODO Auto-generated method stub

    }

    @Override
    public void keyTyped(KeyEvent ke) {
	// TODO Auto-generated method stub

    }

}
