package tralesld;

import tralesld.gui.*;

import tralesld.struct.chart.*;

public class TraleSld
{
    TraleSldGui gui;
    
    //current chart model
    public ChartModel curCM;
    
    public TraleSld()
    {
        gui = TraleSldGui.createAndShowGUI(this);
    }
}
