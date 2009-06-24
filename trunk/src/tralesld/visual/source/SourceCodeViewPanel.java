package tralesld.visual.source;

import java.awt.*;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;

import javax.swing.*;
import javax.swing.text.*;
import java.util.*;
import tralesld.struct.source.*;

public class SourceCodeViewPanel extends JPanel implements ComponentListener
{
	HashMap<String, SourceFileModel> files;
	
	JTextArea codePane;
	JScrollPane codeScrollPane;
	
	public SourceCodeViewPanel()
	{
		codePane = new JTextArea();
		codePane.setEditable(false);
		codePane.setLineWrap(false);
		codeScrollPane = new JScrollPane(codePane);
		this.add(codeScrollPane);
		files = new HashMap<String, SourceFileModel>();
		this.addComponentListener(this);
	}
	
	public void displaySourceCodeLocation(SourceCodeLocation loc)
	{
		SourceFileModel sourceModel = files.get(loc.absolutePath);
		if (sourceModel == null)
		{
			sourceModel = new SourceFileModel(loc.absolutePath);
			files.put(loc.absolutePath, sourceModel);
		}
		System.err.println("Show code location: " + loc.absolutePath + ", line " + loc.lineNumber);
		TextWithMarking textWithMarking = sourceModel.getCompleteContentWithLineOffsets(loc.lineNumber);
		codePane.setText(textWithMarking.text);
		try
		{
			codePane.getHighlighter().addHighlight(textWithMarking.beginIndex, textWithMarking.endIndex, DefaultHighlighter.DefaultPainter);
			codePane.setCaretPosition(textWithMarking.caretIndex);
		}
		catch(BadLocationException ble) 
		{
			System.err.println("Bad location during highlighting!");
		}
	}

    @Override
    public void componentHidden(ComponentEvent e)
    {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void componentMoved(ComponentEvent e)
    {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void componentResized(ComponentEvent e)
    {
        Dimension newSize = new Dimension(this.getParent().getWidth() - 30, this.getParent().getHeight() - 30);
        //codeScrollPane.setMinimumSize(newSize);
        codeScrollPane.setPreferredSize(newSize);
        codeScrollPane.setMaximumSize(newSize); 
    }

    @Override
    public void componentShown(ComponentEvent e)
    {
        // TODO Auto-generated method stub
        
    }
}
