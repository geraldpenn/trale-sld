# What this is about #

This page contains various comments which came up while playing
with the SLD. Only open issues are kept around.

# Issues #

  * definite clauses: unification of arguments is currently presented as if it belonged to trying to execute the first matching goal; from the user's perspective it seems more prominent that the same unification is at work for all alternatives reached by backtracking later. The graphical rendering should reflect this somehow (discussed on June 6th)

  * should potential later backtracking points (and their number) be marked when a definite clause is entered? Note: later cuts might of course eliminate some of these backtracking points.

  * it is not yet easily visible that alternative rule applications during parsing are daughters of a common mother node (discussed in our meeting on June 6th - solution postponed (new drawing algorithm, not urgent))