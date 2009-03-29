package tralesld.mockup;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridLayout;
import java.awt.Image;
import java.awt.image.ImageObserver;
import java.io.IOException;
import java.net.URL;

import javax.imageio.ImageIO;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.MutableTreeNode;

import tralesld.gui.icons.IconUtil;

public class Mockup extends JPanel {

    /**
     * 
     */
    private static final long serialVersionUID = -1328215154091285449L;

    public Mockup() {
	super(new GridLayout(1, 0));
	add(createVerticalSplit());
    }

    private JComponent createVerticalSplit() {
	JSplitPane result = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
		createLeftPanel(), createRightPanel());
	result.setResizeWeight(1);
	return result;
    }

    private JComponent createLeftPanel() {
	JSplitPane result = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
		createStepDetailPanel(), createChartPanel());
	result.setPreferredSize(new Dimension(800, 768));
	result.setResizeWeight(1);
	return result;
    }

    private JComponent createRightPanel() {
	JSplitPane result = new JSplitPane(JSplitPane.VERTICAL_SPLIT,
		createControlPanel(), createGrammarPanel());
	result.setPreferredSize(new Dimension(224, 768));
	return result;
    }

    private JComponent createStepDetailPanel() {
	JTabbedPane result = new JTabbedPane();
	result.addTab("apply head_subject rule", createStepDetailTab());
	return result;
    }

    private JComponent createChartPanel() {
	JTabbedPane result = new JTabbedPane();
	result.setPreferredSize(new Dimension(800, 300));
	result.addTab("Chart", createChartTab());
	return result;
    }

    private JComponent createControlPanel() {
	JTabbedPane result = new JTabbedPane();
	result.setPreferredSize(new Dimension(224, 384));
	result.addTab("Control", createControlTab());
	return result;
    }

    private JComponent createGrammarPanel() {
	JTabbedPane result = new JTabbedPane();
	result.addTab("Signature", createSignatureTab());
	result.addTab("Constraints", createConstraintsTab());
	result.addTab("Rules", createRulesTab());
	return result;
    }

    private JComponent createStepDetailTab() {
	return new JPanel(); // TODO
    }

    private JComponent createChartTab() {
	JPanel result = new JPanel();
	result.setLayout(new BoxLayout(result, BoxLayout.Y_AXIS));
	result.add(createChartControlPanel());
	ImagePanel imagePanel = new ImagePanel(Mockup.class
		.getResource("chart_sample.png"));
	JScrollPane scrollPane = new JScrollPane(imagePanel);
	scrollPane.setBackground(Color.WHITE);
	result.add(scrollPane);
	return result;
    }

    private JComponent createControlTab() {
	JPanel result = new JPanel();
	result.setLayout(new BoxLayout(result, BoxLayout.Y_AXIS));
	result.add(createControlButtonsPanel());
	result.add(createStepsTreePane());
	return result;
    }

    private JComponent createSignatureTab() {
	JPanel result = new JPanel();
	return result;
    }

    private JComponent createConstraintsTab() {
	JPanel result = new JPanel();
	return result;
    }

    private JComponent createRulesTab() {
	JPanel result = new JPanel();
	return result;
    }

    private JComponent createChartControlPanel() {
	JPanel result = new JPanel();
	result.setLayout(new BoxLayout(result, BoxLayout.X_AXIS));
	result.add(new JCheckBox("show junk edges", true));
	result.add(Box.createHorizontalGlue());
	return result;
    }

    private JComponent createControlButtonsPanel() {
	JPanel result = new JPanel();
	result.setLayout(new BoxLayout(result, BoxLayout.X_AXIS));
	result.add(createButton("creep.png", "continue"));
	result.add(Box.createHorizontalGlue());
	result.add(createButton("skip.png", "auto-complete this step"));
	result.add(Box.createHorizontalGlue());
	result.add(createButton("reject.png", "make this step fail"));
	result.add(Box.createHorizontalGlue());
	result.add(createButton("leap.png", "auto-complete"));
	result.add(Box.createHorizontalGlue());
	return result;
    }

    private JComponent createStepsTreePane() {
	return new JScrollPane(createStepsTreeView());
    }

    private JComponent createStepsTreeView() {
	MutableTreeNode root = createTreeNode(
		new Step(Step.STATUS_PROGRESS, "parse string \"it walks\"", 0));
	JTree result = new JTree(root);
	result.setCellRenderer(new StepRenderer());
	return result;
    }

    private JButton createButton(String filename, String toolTipText) {
	JButton result = new JButton(new ImageIcon(IconUtil.getIcon(filename)));
	result.setToolTipText(toolTipText);
	return result;
    }

    private MutableTreeNode createTreeNode(Object userObject) {
	return createTreeNode(userObject, new MutableTreeNode[0]);
    }

    private MutableTreeNode createTreeNode(Object userObject,
	    MutableTreeNode[] children) {
	DefaultMutableTreeNode result = new DefaultMutableTreeNode(userObject);

	for (MutableTreeNode child : children) {
	    result.add(child);
	}

	return result;
    }

    private class StepRenderer extends DefaultTreeCellRenderer {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8045182470829809316L;

	private Icon progressIcon = new ImageIcon(IconUtil
		.getIcon("progress.png"));

	private Icon successIcon = new ImageIcon(IconUtil
		.getIcon("success.png"));

	private Icon failureIcon = new ImageIcon(IconUtil
		.getIcon("failure.png"));

	public JComponent getTreeCellRendererComponent(JTree tree,
		Object value, boolean sel, boolean expanded, boolean leaf,
		int row, boolean hasFocus) {
	    super.getTreeCellRendererComponent(tree, value, sel, expanded,
		    leaf, row, hasFocus);

	    if (!(value instanceof DefaultMutableTreeNode)) {
		return this;
	    }

	    Object userObject = ((DefaultMutableTreeNode) value)
		    .getUserObject();

	    if (!(userObject instanceof Step)) {
		return this;
	    }

	    Step step = (Step) userObject;
	    int status = step.getStatus();

	    if (status == Step.STATUS_PROGRESS) {
		setIcon(progressIcon);
	    } else if (status == Step.STATUS_SUCCESS) {
		setIcon(successIcon);
	    } else if (status == Step.STATUS_FAILURE) {
		setIcon(failureIcon);
	    }

	    setText(step.getText());

	    return this;
	}

    }

    private class ImagePanel extends JPanel {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5901199359108764061L;

	private URL url;

	private Image image;

	private int width = -1;

	private int height = -1;

	public ImagePanel(URL imageURL) {
	    this.url = imageURL;
	}

	private void loadImage() {
	    try {
		image = ImageIO.read(url);
		width = image.getWidth(new ImageObserver() {

		    @Override
		    public boolean imageUpdate(Image img, int infoflags, int x,
			    int y, int newWidth, int newHeight) {
			System.err.print("1 ");
			if (newWidth == -1) {
			    return false;
			}

			width = newWidth;
			return true;
		    }

		});
		height = image.getHeight(new ImageObserver() {

		    @Override
		    public boolean imageUpdate(Image img, int infoflags, int x,
			    int y, int newWidth, int newHeight) {
			System.err.print("2 ");
			if (newHeight == -1) {
			    return false;
			}

			height = newHeight;
			return true;
		    }

		});
	    } catch (IOException e) {
		e.printStackTrace();
	    }
	}

	public void paintComponent(final Graphics g) {
	    // work around bug https://bugs.launchpad.net/ubuntu/+source/sun-java6/+bug/250931
	    try {
		Thread.sleep(10);
	    } catch (InterruptedException e) {
	    }
	    
	    if (image == null) {
		loadImage();
	    }

	    if (width == -1 || height == -1) {
		super.paint(g);
		return;
	    }

	    Dimension dimension = new Dimension(width, height);
	    setSize(dimension);
	    g.drawImage(image, 0, 0, new ImageObserver() {

		@Override
		public boolean imageUpdate(Image img, int infoflags, int x,
			int y, int width, int height) {
		    System.err.print("3 ");
		    g.drawImage(img, 0, 0, this);
		    return true;
		}

	    });
	    
	    g.dispose();
	}

	public boolean imageUpdate(Image img, int infoflags, int x, int y,
		int width, int height) {
	    this.width = width;
	    this.height = height;
	    return super.imageUpdate(img, infoflags, x, y, width, height)
		    && width != -1 && height != -1;
	}

    }

    private static void createAndShowGUI() {
	// Create and set up the window.
	JFrame frame = new JFrame("TraleSLD");
	frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

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
