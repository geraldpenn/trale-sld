package tralesld;

import tralesld.gui.*;

import tralesld.storage.*;
import tralesld.struct.chart.*;

public class TraleSld
{
    TraleSldGui gui;
    
    //current chart model
    public ChartModel curCM;
    
    public DataStore<ChartModelChange> chartChanges; 
    
    public TraleSld()
    {
        gui = TraleSldGui.createAndShowGUI(this);
    }
}
