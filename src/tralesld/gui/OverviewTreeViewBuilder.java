package tralesld.gui;

import tralesld.visual.tree.*;
import tralesld.*;

public class OverviewTreeViewBuilder
{
    public TreeView createOverviewTreeView(TraleSld sld)
    {
        TreeView t = new TreeView(sld.tracer.overviewTraceModel);
        return t;
    }
}
