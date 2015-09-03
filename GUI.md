# Introduction #

Here's an initial mockup for the GUI of the graphical debugger - a mere suggestion as of now, for discussion, may change radically.

![http://trale-sld.googlecode.com/svn/wiki/GUIMockupDraft.png](http://trale-sld.googlecode.com/svn/wiki/GUIMockupDraft.png)

In this draft, the screen is divided into four principal areas (clockwise, starting at the top right): The Control Panel, the Grammar Panel, the Chart Panel, and the Step Detail Panel.

About the TRALE grammar assumed in this example, suffice it to say that it is HPSG-like with two rules, the head complement rule and the head subject rule. Also, there is two different lexical entries for the word _it_.

At the point of parsing shown, TRALE's parser has already added a number of edges to the chart, and is now about to try to apply the head complement rule to edges 3 and 0 (which will fail).

# The Control Panel #

The Control Panel contains a tree view of the parsing process, where a step that is executed as part of the execution of another step is represented as a descendant of this step. Apart from the root, representing the whole parsing process, there is three kinds of steps: Adding edges to the chart, closing newly added edges under rule application, and applying a specific rule.

TODO:
  * It shouldn't just say "apply rule XYZ", but more specifically "apply rule XYZ to edges A and B"

Steps that have completed successfully are marked with a green checkmark, steps that have failed are marked with a red ballot, and steps that are currently in the process of execution are marked with an hourglass.

This style of presentation seems most convenient for me for TRALE's parsing algorithm, which is bottom-up and does not use backtracking in any way interesting to the user. This is different on the finer level of enforcing descriptions and principles on feature structures and resolving definite clause goals - here it might turn out useful to use a decision tree, although this would add considerable complexity from the user's point of view.

TODO
  * Investigate, discuss this.

In the current draft, there is four buttons for the user to control the parsing process (from left to right):
  * **Creep:** go to the next step
  * **Skip:** auto-complete the current step, don't stop at sub-steps
  * **Fail:** do not try to complete the current step, act as if the step had failed
  * **Leap:** auto-complete the whole parsing process.

The latter could be what one wants to do most of the time, since the graphical debugger will allow for reviewing all parsing steps in full detail afterwards. Specifically, clicking on steps in the tree view will show the respective parsing state in the Chart and Step Detail view. The only scenario where this is not an option is when there's bugs in the grammar that cause the parser to loop.

# The Grammar Panel #

Provides a hierarchical view of the signature, and lists of rules and principles. Left blank for the moment since this is Flar's domain.

TODO:
  * Define interactive functionality - for example, selecting a rule application step could highlight the respective rule, clicking on a rule could highlight the edges introduced by that rule, etc.

# The Chart Panel #

This visualizes edges already on the chart (green), the edge that will be added if the current rule application succeeds (blue), and, optionally, "junk edges", i.e. edges that would have been added had a previous rule application succeeded. "Junk edges" are a key idea towards a "stateless" approach to debugging: clicking on a junk edge will show bring the details of that rule application step back to the screen. This makes it easy for the user to find out precisely why a particular rule application failed.

The chart is right-aligned, corresponding to TRALE's right-to-left approach to chart parsing.

# The Step Detail Panel #

This is the part currently farthest from what I would like it to look like. In principle, understanding the application of a rule with two categories on the right-hand side may require looking at four AVMs at once: the descriptions from the rule, and the feature structures from the chart that the descriptions are enforced upon. Together with definite clause goals, which one might want to represent as boxes, this is a challenge even on very large screens. Reducing visual complexity in such a way that it aids understanding might be a very interesting challenge here.

One idea I head after finalizing the screenshot is laying out the AVMs of the rule and those of the described features structures out in two columns, such as to have the describing and the described AVM next to each other, and to allow a vertical scrollbar. In any case, the AVMs should be arranged more neatly in rows and columns, and the visual impression of rows and columns should be supported by background colors or something.

Colors can also be used to visualize the connection between feature structures and the corresponding chart edges, already attempted in the current draft using colored boxes.

Another idea to help the user, already realized in the current draft: Allow the original Prolog variable names for index tags (not just numbers).

For the draft, I've created the AVMs in LaTeX. This will of course be taken over by (possibly modified) components from Gralej.

TODO:
  * think of a way to convey the enforcement of constraints visually
  * add an module which provides an overview over variable bindings
  * spiffy visualization of bound variables/re-entrancies - clicking on one tag should highlight all corresponding tags
  * visual tracking of changes in the AVMs