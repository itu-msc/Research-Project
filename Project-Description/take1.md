# FRP with simplified LTL

> Situation:
Functional reactive programming (FRP) is used in applications such as servers, games, and GUIs but incorrect use leads to non-causal (future-dependent) definitions and space leaks in long-running programs.

> Complication:
Previous works introduced modal types to address the issues with space leaks in FRP. Where modal types describe when a value is available (now or later). For example, the type `⃝int` describes a promise of an integer value that will be available later. `square int` describes an integer value that is time independent, making it available now and at all future times.

However having several modal types comes at a cost of making the type system more restrictive and complex. Which makes it less practical for real world applications.

> Proposal:
A new simplified approach has been machine checked that removes the need for boxing of types.
We want to make a library implementation as a proof of concept, to show that this approach is practical and can be used to solve the space leak problem in FRP.
