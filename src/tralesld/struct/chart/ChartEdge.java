package tralesld.struct.chart;

public class ChartEdge
{
	public static int number;
	
    public int id;
    public int l;
    public int r;
    public String desc;
    public int status;
    public boolean active;
    
    public static final int FAILED = 0;
    public static final int SUCCESSFUL = 1;
    public static final int ACTIVE = 2;
    
    public ChartEdge(int l, int r, String desc, int status, boolean active)
    {
        this.id = number;
    	number++;
        this.l = l;
        this.r = r;
        this.desc = desc;
        this.status = status;
        this.active = active;
    }
    
    public boolean equals(Object o)
    {
        if (o instanceof ChartEdge)
        {
            ChartEdge oe = (ChartEdge) o;
            if (oe.l != l) return false;
            if (oe.r != r) return false;
            if (!desc.equals(oe.desc)) return false;
            if (oe.status != status) return false;
            return true;
        }
        else
        {
            return false;
        }
    }
    
    public String toString()
    {
    	return "[" + id + "," + l + "," + r + "," + desc + "," + status + "," + active + "]";
    }
}
