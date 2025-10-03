# FRP with simplified LTL

> Situation:

Functional reactive programming (FRP) is a programming paradigm for building programs that react to input from the environment in a functional style. Examples of such programs include servers, games, and GUIs. FRP models the interaction with the environment through *signals*, which are time-dependent values that work like streams of values that show up later. Incorrectly using signals may lead to space leaks in long-running programs.

> Complication:

Previous works introduced modal types to address the issues with space leaks in FRP. Modal types describe when a value is available (now or later). For example, the type `⃝int` describes a promise of an integer value that will be available later. `▢int` describes an integer value that is time independent, making it available now and at all future times.

However, having several modal types comes at a cost of making the type system more restrictive and complex. Which makes it less practical for real world applications.

> Proposal:

A new recently proposed FRP language simplifies the type system while preserving the safety guarantees. Our goal is to demonstrate that this new language can be implemented as an embedded language. We will do it by making a library implementation in an existing programming language.





Functional reactive programming (FRP) is a programming paradigm for building programs that react to input from the environment in a functional style. Examples of such programs include servers, games, and GUIs. FRP models the interaction with the environment through *signals*, which are time-dependent values that function like streams of values that emerge later. Incorrectly using signals may lead to space leaks in long-running programs.

Previous works introduced modal types to address the issues with space leaks in FRP. Modal types describe when a value is available (now or later). For example, the type `οint` describes a promise of an integer value that will be available later. `▢int` describes a time-independent integer value, making it available now and at all future times.

However, having several modal types comes at a cost of making the type system more restrictive and complex. Which makes it less practical for real-world applications.

A recently proposed FRP language simplifies the type system while preserving the safety guarantees. Our goal is to demonstrate that this new language can be implemented as an embedded language. We will do it by making a library implementation in an existing programming language.
