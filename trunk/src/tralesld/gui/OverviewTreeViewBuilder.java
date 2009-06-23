package tralesld.gui;

import tralesld.visual.tree.*;
import tralesld.*;

public class OverviewTreeViewBuilder
{
    public TreeView createOverviewTreeView(TraleSld sld)
    {
        return sld.tracer.overviewTraceView;
    }
}
