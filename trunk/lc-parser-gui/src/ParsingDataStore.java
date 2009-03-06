import java.util.*;

public class ParsingDataStore<T>
{
    ArrayList<T> parsingData;
    
    public ParsingDataStore()
    {
        parsingData = new ArrayList<T>();
    }
    
    public int storeNewData(T data)
    {
        parsingData.add(data);
        return parsingData.size() - 1;
    }
    
    public T getData(int id)
    {
        if (id == -1)
        {
            return null;
        }
        else
        {
            return parsingData.get(id);
        }
    }
}
