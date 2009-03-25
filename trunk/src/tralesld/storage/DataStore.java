package tralesld.storage;
import java.util.*;

public class DataStore<T>
{
    ArrayList<T> parsingData;
    
    public DataStore()
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
