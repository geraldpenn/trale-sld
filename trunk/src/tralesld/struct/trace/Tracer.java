package tralesld.struct.trace;
public class Tracer
{
    XMLTraceModel traceModel;
    
    public Tracer()
    {
        traceModel = new XMLTraceModel();
    }
    
    public int registerStep(String stackList, int content, String shortDescription)
    {
        return traceModel.extendModel(null,content, shortDescription);
    }
}
