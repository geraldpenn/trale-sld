public class ParseTracer
{
    ParseTraceModel traceModel;
    
    public ParseTracer()
    {
        traceModel = new ParseTraceModel();
    }
    
    public int registerStep(String stackList, int content, String shortDescription)
    {
        return traceModel.extendModel(PrologUtilities.parsePrologIntegerList(stackList),content, shortDescription);
    }
}
