package tralesld.visual.source;

import java.awt.*;

import javax.swing.*;
import javax.swing.text.*;
import java.util.*;
import tralesld.struct.source.*;

public class SourceCodeViewPanel extends JPanel
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
		codeScrollPane.setMinimumSize(new Dimension(300,300));
		codeScrollPane.setPreferredSize(new Dimension(300,300));
		codeScrollPane.setMaximumSize(new Dimension(300,300));
		this.add(codeScrollPane);
		files = new HashMap<String, SourceFileModel>();
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
			codePane.getHighlighter().addHighlight( textWithMarking.beginIndex, textWithMarking.endIndex, DefaultHighlighter.DefaultPainter);
			codePane.setCaretPosition(textWithMarking.caretIndex);
		}
		catch(BadLocationException ble) 
		{
			System.err.println("Bad location during highlighting!");
		}
	}
}
