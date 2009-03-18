package tralesld.mockup;

public class Step {
    
    public static final int STATUS_PROGRESS = 1;
    
    public static final int STATUS_SUCCESS = 2;
    
    public static final int STATUS_FAILURE = 3;
    
    private int status;
    
    private String text;
    
    public Step(int status, String text) {
	this.status = status;
	this.text = text;
    }
    
    public int getStatus() {
	return status;
    }
    
    public String getText() {
	return text;
    }

}
