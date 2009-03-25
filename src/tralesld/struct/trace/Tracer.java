package tralesld.struct.trace;

import java.util.*;

public class Tracer
{
    public XMLTraceModel traceModel;
    
    public Tracer()
    {
        traceModel = new XMLTraceModel();
    }
    
    public XMLTraceNode registerStep(List<Integer> stackList, int content, String shortDescription)
    {
        return traceModel.extendModel(stackList, content, shortDescription);
    }
}
