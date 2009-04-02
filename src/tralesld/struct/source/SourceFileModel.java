package tralesld.struct.source;

import java.util.*;
import java.io.*;

public class SourceFileModel 
{
	String absolutePathName; 
	List<String> lines;
	
	public SourceFileModel(String absolutePathName)
	{
		this.absolutePathName = absolutePathName;
		File sourceFile = new File(absolutePathName);
		lines = new ArrayList<String>();
		try
		{
			Scanner sourceFileScanner = new Scanner(sourceFile);
			while (sourceFileScanner.hasNextLine())
			{
				lines.add(sourceFileScanner.nextLine());
			}
		}
		catch (FileNotFoundException e)
		{
			lines.add("ERROR: could not load source file " + absolutePathName);
			System.err.println("ERROR: could not load source file " + absolutePathName);
		}		
	}
	
	public String getLineContent(int lineNumber)
	{
		String result = lines.get(lineNumber);
		if (result == null) result = "";
		return result;
	}
	
	public String getCompleteContent()
	{
		StringBuilder builder = new StringBuilder("");
		for (String line : lines)
		{
			builder.append(line);
		}
		return builder.toString();
	}
	
	public TextWithMarking getCompleteContentWithLineOffsets(int lineNumber)
	{
		int beginOffset = 0;
		int endOffset = 0;
		int caretIndex = 0;
		StringBuilder builder = new StringBuilder("");
		for (int i = 0; i < lines.size(); i++)
		{
			if (i == lineNumber)
			{	
				beginOffset = builder.length();
				builder.append(lines.get(i) + "\n");
				endOffset = builder.length();
			}
			else if (i == lineNumber - 3)
			{
				caretIndex = builder.length();
				builder.append(lines.get(i) + "\n");
			}
			else
			{
				builder.append(lines.get(i) + "\n");
			}
		}
		return new TextWithMarking(builder.toString(),beginOffset,endOffset,caretIndex);
	}
}
