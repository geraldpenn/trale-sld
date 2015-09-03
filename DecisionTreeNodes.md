Nodes in the DecisionTree correspond to steps in the parsing process. Their labels are inspired by the compact internal representation of the TRALE debugger. Here's an overview over the most frequently occurring node labels and their meanings.

Capitalized words in the left column represent variables that are replaced by applicable values for each node.

| parsing | The root node. |
|:--------|:---------------|
| rule\_close | Every such node corresponds to a chart edge. It represents the process of closing this chart edge under rule application, i.e. adding every possible chart edge where this chart edge is the left corner to the chart. This is done by applying rules (see next row) and recursively closing any resulting new edges under rule application. |
| rule(RuleName) | Application of a particular PS rule to a particular edge, where this edge is in the left corner of the rule body. |
| unify(VariableName) | Unification of a feature structure with a variable. The variable name is displayed as it occurs in the grammar. |
| featval(Path:FeatureName) | Unification of feature values, "recursive case": A description selects a feature via a path. |
| type(Location,Type) | Unification of feature values, "base case": A type is added to the value of a specific feature. |