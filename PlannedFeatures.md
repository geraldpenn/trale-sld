# Visualization #

  * of the grammar:
    * ✓ display theory file and highlight line corresponding to current step
    * ⌛ sort hierarchy with attribute appropriateness
  * of the parsing process:
    * ✓ interactive decision tree
      * ✓ basic structure based on control flow (branches during backtracking)
      * ✓ secondary tree based on call stacks
      * ✓ visualize jumps after exits and failures
      * ⌛ allow zooming for tree navigation
      * ✓ chart edges linked to nodes in decision tree
    * ✓ chart display
      * ✓ interactively show dependencies between "daughter" and "mother" chart edges
    * ✓ always show parse tree fragment of current rule application
      * ✓ inspect feature structures (GraleJ)
      * ⌛ highlight substructures affected by current step
    * ✓ extend decision tree to logic programs
      * ⌛ display variable bindings at each step
    * ⌛ show suspended goals
      * ⌛ for each variable involved in current step, show lurking goals
      * ⌛ visualize interaction of suspended goals

# Analysis #

  * of the grammar:
    * ⌛ analyze arity of PS rules (for parse tree fragments, detection of failure causes, maximum chart width etc.)
  * of the parsing process:
    * ✓ overview tree (allows inspection of chart construction process)
    * ⌛ profiling capabilities
      * ⌛ statistics for predicate calls under each decision tree node
      * ⌛ statistics for rule applications under each decision tree node

# Control #

  * ✓ steer the tracer via GUI buttons
    * ✓ keyboard shortcuts
  * ⌛ break point system
    * ⌛ define breakpoints via decision tree templates
    * ⌛ add leap capability
      * ⌛ less frequent GUI repainting when leaping (speed!)
  * ⌛ logging and restoring
    * ⌛ log user commands during parsing process, allow export
    * ⌛ reconstruct parser states based on log import

# Architecture #

  * ✓ communication between Prolog and Java via Jasper interface
  * ✓ modular, facility to add visualization components
  * ✓ independent threads for visualization and control to increase responsiveness
  * ⌛ allow parallel control via GUI and console as soon as non-blocking I/O is possible (SICStus 4)

# Advanced Features #

_(will probably require extending TRALE itself a lot, to be investigated)_

  * direct links from generated (sub)structures to the principles/ID schemata/signature entries/... that licensed those structures
  * ability to Retry a specific step (other than the one that has just completed)