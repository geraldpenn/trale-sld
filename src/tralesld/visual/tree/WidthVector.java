package tralesld.visual.tree;

import java.util.ArrayList;

public class WidthVector
{
    ArrayList<Integer> start;
    ArrayList<Integer> end;
    
    public WidthVector()
    {
        start = new ArrayList<Integer>();
        start.add(1);
        end = new ArrayList<Integer>();
        end.add(1);
    }
    
    public static WidthVector adjoin(WidthVector w1, WidthVector w2)
    {
        //System.err.println("Adjoining: " + w1 + " and " + w2);
        WidthVector w3 = new WidthVector();
        w3.start.clear();
        w3.end.clear();
        int w1size = w1.start.size();
        int w2size = w2.start.size();
        int minSize = w1size;
        if (w1size > w2size) minSize = w2size;
        int maxReqDistance = 0;
        int maxReqDistanceLevel = 0;
        for (int i = 0; i < minSize; i++)
        {
            int reqDistance = w1.end.get(i) + w2.start.get(i);
            if (reqDistance > maxReqDistance)
            {
                maxReqDistance = reqDistance;
                maxReqDistanceLevel = i;
            }
        }
        int leftOffset = w1.end.get(maxReqDistanceLevel);
        int rightOffset = w2.start.get(maxReqDistanceLevel);
        for (int i = 0; i < minSize; i++)
        {
            w3.start.add(w1.start.get(i) + leftOffset);
            w3.end.add(w2.end.get(i) + rightOffset);
        }
        for (int i = minSize; i < w1size; i++)
        {
            w3.start.add(w1.start.get(i) + leftOffset);
            w3.end.add(w1.end.get(i) - leftOffset);
        }
        for (int i = minSize; i < w2size; i++)
        {
            w3.start.add(w2.start.get(i) - rightOffset);
            w3.end.add(w2.end.get(i) + rightOffset);
        }
        //System.err.println("Result: " + w3);
        return w3;
    }
    
    public double maximumLeftDistance()
    {
        //System.err.println("Determine left distance for " + toString());
        double maximum = 1;
        int size = start.size();
        for (int i = 0; i < size; i++)
        {
            int val = start.get(i);
            if (val > maximum) maximum = val;
        }
        //System.err.println("Result: " + maximum);
        return maximum;
    }
    
    public double maximumRightDistance()
    {
        double maximum = 1;
        int size = end.size();
        for (int i = 0; i < size; i++)
        {
            int val = end.get(i);
            if (val > maximum) maximum = val;
        }
        return maximum;
    }
    
    public static int computeNecessaryDistance(WidthVector w1, WidthVector w2)
    {
        //System.err.println("Distance between: " + w1 + " and " + w2);
        int w1size = w1.start.size();
        int w2size = w2.start.size();
        int minSize = w1size;
        if (w1size > w2size) minSize = w2size;
        int maxReqDistance = 0;
        for (int i = 0; i < minSize; i++)
        {
            int reqDistance = w1.end.get(i) + w2.start.get(i);
            if (reqDistance > maxReqDistance)
            {
                maxReqDistance = reqDistance;
            }
        }
        //System.err.println("Result: " + maxReqDistance);
        return maxReqDistance;
    }
    
    public WidthVector copy()
    {
        WidthVector copy = new WidthVector();
        copy.start.clear();
        copy.end.clear();
        copy.start.addAll(start);
        copy.end.addAll(end);
        return copy;
    }
    
    public String toString()
    {
        int size = start.size();
        String str = "";
        for (int i = 0; i < size; i++)
        {
            str += "["+ start.get(i) + "," + end.get(i) + "]";
        }
        return str;
    }
}
