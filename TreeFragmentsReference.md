Note: The structure used to talk about dependencies between steps in this section is the TRALE call tree, not the decision tree.

Each parse tree fragment displayed in the step detail view corresponds to a rule application and is of depth 2: The root corresponds to the mother category, the children correspond to the daughter categories. Their descendants, if any, are not shown.

The tree fragment shown is the same for a rule application step and all of its descendants (in the call tree). However, there can be changes in the individual nodes (feature structures) during the process as unfications take place (features are added, highlighted, unhighlighted, changes are reverted in backtracking...), so each step warrants an individual picture.

In order to create a picture for a step, trale-sld needs to know
  1. which rule application the step belongs to
  1. the name of the rule
  1. the number N of daughter categories (i.e. the length of the right-hand side of the rule)
  1. how many of the daughter categories are already associated with chart edges in the step, i.e. some i between 1 and N (association of daughter categories with chart edges proceeds in a strictly left-to-right manner)
  1. the indexes (IDs) of these edges
  1. the part of the input covered by the daughter categories currently already associated with chart edges (this is a prefix of the part of the input that will eventually be covered by the rule application if it is successful)
  1. the feature structures associated with the edges associated with the daughter categories
  1. the feature structure associated with the mother category
  1. which part of which feature structure is affected by the current step (for highlighting)

Here's how this information is obtained, respectively:
  1. asserta the StepID of the rule application step (RAID) when entering (call, redo) a rule application step, retractall it when leaving (exit, fail, finished) the step. The current RAID is then always on top of the "assertion stack".
  1. store name in said assertion
  1. store number in said assertion
  1. make an assertion storing the initial number (1) when entering a rule application step; increment this number (using retract, assert) whenever an edge is retrieved (announce\_edge\_retrieved\_hook/1).
    * also, to make this backtracking-safe: when entering a step through the call port, map it (in some asserted structure) to the currently asserted number; when re-entering a step through the redo port, look up the associated number and assert it to be the current one again (strictly speaking, we only need to take these extra measures for children of rule application steps since edge-retrievals always take place between these -> we're guaranteed to see the redo port of one of them whenever the control flow goes back past an edge-retrieval)
  1. assert a mapping from daughter positions to edge indexes (IDs); clear it at the latest when leaving the rule application step
  1. use the indexes in aforementioned mapping and the wonderful get\_edge\_ref/6 to recursively go to the rightmost leaf covered by the current daughter edges, from whose position and the input "string" (word list) the prefix can be calculated...
  1. just look them up using the indexes and get\_edge\_ref/6
  1. TBD
  1. TBD

Rule application steps are only ever left through the fail port -> they are never redone -> we don't need to worry about preserving our asserted extra knowledge beyond an execution of a rule application step.