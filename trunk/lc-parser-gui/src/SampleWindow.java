import javax.swing.*;

public class SampleWindow extends JFrame
{
	//serialVersionUID to avoid warning
	private static final long serialVersionUID = 1L;
	
	String displayedContent;
	JLabel contentLabel;
	
	public SampleWindow(String displayedContent)
	{
		this.displayedContent = displayedContent;	
		contentLabel = new JLabel(displayedContent);
		this.getContentPane().add(contentLabel);
	}
}
