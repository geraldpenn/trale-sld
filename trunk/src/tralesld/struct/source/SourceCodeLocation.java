package tralesld.struct.source;

public class SourceCodeLocation 
{
	public String absolutePath;
	public int lineNumber;
	
	public SourceCodeLocation(String absolutePath, int lineNumber)
	{
		this.absolutePath = absolutePath;
		this.lineNumber = lineNumber;
	}
}
