# FRP with simplified LTL

> Situation:
FRP is used in applications such as servers, games, and GUIs but incorrect use leads to non-causal (future-dependent) definitions and space leaks in long-running programs.

> Complication:
Previous works introduced boxing of types to make them stable, in the goal of solving the space leak problem in FRP.
However boxing of types in FRP comes at a cost of making the type system more restrictive and complex. Which makes it less practical for real world applications.

> Proposal:
A new simplified approach has been machine checked that removes the need for boxing of types.
We want to make a library implementation as a proof of concept, to show that this approach is practical and can be used to solve the space leak problem in FRP.
