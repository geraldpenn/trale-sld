package tralesld.storage;
import java.util.*;

public class DataStore<T>
{
    HashMap<Integer,T> parsingData;
    
    public DataStore()
    {
        parsingData = new HashMap<Integer,T>();
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
    
    public void put(int index, T item)
    {
    	parsingData.put(index, item);
    }
}
