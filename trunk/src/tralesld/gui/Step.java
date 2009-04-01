package tralesld.gui;

public class Step
{

    public static final int STATUS_PROGRESS = 2;

    public static final int STATUS_SUCCESS = 1;

    public static final int STATUS_FAILURE = 0;

    private int status;

    private String text;

    private int stepID;

    public Step(int status, String text, int stepID)
    {
        this.status = status;
        this.text = text;
        this.stepID = stepID;
    }

    public int getStatus()
    {
        return status;
    }
    
    public void setStatus(int status)
    {
        this.status = status;
    }

    public String getText()
    {
        return text;
    }
    
    public void setText(String text)
    {
    	this.text = text;
    }

    public int getStepID()
    {
        return stepID;
    }

    public String toString()
    {
    	return "[" + stepID + "," + text + "," + status + "]";
    }
}
