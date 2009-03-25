import java.util.*;

public class PrologUtilities
{
    public static List<Integer> parsePrologIntegerList(String prologList)
    {
        List<Integer> result = new LinkedList<Integer>();
        StringBuffer charAggregator = new StringBuffer();
        char[] chars = prologList.toCharArray();
        for (int i = 1; i < chars.length - 1; i++)
        {
            if (chars[i] == ',')
            {
                result.add(Integer.parseInt(charAggregator.toString()));
                charAggregator = new StringBuffer();
            }
            else
            {
                charAggregator.append(chars[i]);
            }
        }
        if (charAggregator.length() > 0)
        {
            result.add(Integer.parseInt(charAggregator.toString()));
        }
        return result;
    }
    
    public static List<String> parsePrologStringList(String prologList)
    {
        List<String> result = new LinkedList<String>();
        StringBuffer charAggregator = new StringBuffer();
        char[] chars = prologList.toCharArray();
        for (int i = 1; i < chars.length - 1; i++)
        {
            if (chars[i] == ',')
            {
                result.add(charAggregator.toString());
                charAggregator = new StringBuffer();
            }
            else
            {
                charAggregator.append(chars[i]);
            }
        }
        if (charAggregator.length() > 0)
        {
            result.add(charAggregator.toString());
        }
        return result;
    }
}
