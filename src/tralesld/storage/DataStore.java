package tralesld.storage;

import java.util.*;

public class DataStore<T>
{
	HashMap<Integer, T> parsingData;

	public DataStore()
	{
		parsingData = new HashMap<Integer, T>();
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

	public int size()
	{
		return parsingData.size();
	}

	public Set<Integer> getKeySet()
	{
		return parsingData.keySet();
	}

	public String toString()
	{
		StringBuilder builder = new StringBuilder();
		
		for (int i : parsingData.keySet()) {
			builder.append(i + " => " + parsingData.get(i) + ", ");
		}

		return builder.toString();
	}
}
