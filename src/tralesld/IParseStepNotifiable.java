package tralesld;

/**
 * This interface describes how I currently picture the methods that TRALE will
 * call via Jasper in order to notify trale-sld of the steps of the parsing
 * process. As of yet, it merely serves as a concept draft. One thing which is
 * especially unfathomed is the highly complex and diverse descriptions of the
 * various kinds of parsing steps (see {@link http://www.ale.cs.toronto.edu/docs/man/ale_trale_man/ale_trale_man-node70.html#SECTION0015114000000000000000})
 * and the associated data, and in what form to send them through the interface.
 * Hence "Object step" for now.
 * @author ke
 *
 */
public interface IParseStepNotifiable {
    
    /**
     * Indicates that the debugger is about to invoke a step.
     * @param invocationID A unique identifier of this invocation of the step.
     *     Will subsequently be used for notification at other ports of the
     *     same step. 
     * @param step
     * @param line The associated line in the grammar code file - for eventual
     *     code display in the GUI.
     */
    public void call(int invocationID, Object step, int line);
    
    /**
     * Indicates that the debugger is about to re-invoke a step because the
     * user forced the debugger to "retry" it. This method is there so
     * {@link call(int, Object, int)} does not have to be called again with
     * all the data.
     */
    public void retry(int invocationID);
    
    /**
     * Indicates that the step has failed.
     * @param invocationID
     */
    public void fail(int invocationID);
    
    /**
     * Same as {@link fail(int)}, but indicates failure forced by the user.
     * This distinction may be relevant for presentation.
     * @param invocationID
     */
    public void forcedFail(int invocationID);
    
    /**
     * Indicates that the step has successfully completed.
     * @param invocationID
     */
    public void exit(int invocationID);
    
    /**
     * Indicates that the debugger is about to backtrack into the step to find
     * more solutions.
     * @param invocationID
     */
    public void redo(int invocationID);
    
    /**
     * Indicates that an edge has been added to the chart.
     * @param edgeID
     * @param left
     * @param right
     * @param ruleName
     */
    public void edgeAdded(int edgeID, int left, int right, String ruleName);
    
    /**
     * Indicates that an edge has been retrieved from the chart.
     * @param edgeID
     */
    public void edgeRetrieved(int edgeID);

}
