---
title: "Ironfleet: Proving Practicial Distributed Systems Correct"
author: Cjen1 (Chris Jensen)
---

## Overview

Today I'm looking at the Ironfleet paper by Hawblitzel, Howell et. al [@hawblitzel2015ironfleet].

This paper primarily presents and highlights a series of techniques which make verifying systems easier, a summary of the techniques follows:

1. State Machine Refinement: showing that a low level (L) spec refines a high level (H) spec, ie for every sequence of states in L there is an equivilent correct sequence of behaviors in H.

2. Floyd-Hoard Verification: this is where functions are annotated with pre and post conditions, then prove for each call in the function that the precondition implies the postcondition.

3. Always-Enabled: for every action in the protocol, it is always possible for it to be taken.

4. Round-Robin scheduler: because each action can always be attempted, a round robin scheduler bounds the time until an action is taken.

5. Ghost Variables: variables which appear in the proof but not in the implementation, eg messages received.

6. Invariant Quantifier Hiding: by taking variables as part of the input to the lemma properties can be proved for a given message etc, rather than all messages, which is substantially easier.

Their general approach to verifying systems is to split the workload into three, we'll use a lock server as the working example.

Firstly they take as input some overarching properties that they want to prove. 
For distributed systems this spec generally encompasses a safety property (eg. that only one client holds the lock in a given epoch) and a liveness property (eg if a client requests a lock it will eventually hold it).
They then produce a high level spec (H) which defines a state machine with some initial state, a transition function, and a relation which shows the properties we want to hold at each state.

The idea is then that the protocol layer (P) refines the spec, so they define a function mapping from a state in the protocol layer to a state in the spec layer. This allows for an inductive proof of refinement: initially show that the relation holds for both initial states, then show that each step in P corresponds to some number of valid steps (maybe zero) in H.
This proof is the main workload involving establishing an invariant which holds over the steps.

Then for the implementation layer (I), it needs to be shown to refine the protocol layer. This is done mainly by establishing that each step in P corresponds to some action taken in I. 

There is however an interesting complication in this layer.
That is that although each of the 'actions' can be viewed as atomic inuitively, low level operations can be interleaved between nodes, thus the approach is that of reduciton mentioned above.
That is to prove that these low-level operations can commute because they are based on different hosts, and thus can reorder the execution in order to have coarse grained actions.

One further technique is to initially implement and prove for immutable data structures since that provides greater isolation, before then showing simply that the mutable version is equivalent to the immutable one.

The final wrinkle is that of liveness.
The standard formulation is that if a node can take an action that it eventually will.
The approach taken then is that of the Always-enabled actions, that is that every action is attempted and if not enabled is just skipped. 
That combined with a round robin scheduler (and no infinite loops) results in the desired liveness property as follows.

Furthermore it is desired (for failure detection reasons) that the time until an action is taken is bounded, and from the round robin that is achieved: that is that if there is some rate F at which actions is taken then for `N` actions each action will occur with a frequency of `F/N`.

## Interesting take-aways
They have a couple of additional general 'software engineering practices` which they claim are useful.

* Encapsulation: As in general software engineering encapsulation also aids verification by reducing the number of interactions between components allowing each component to be verified in turn.
* Ghost variables: although these are not actually useful in the implementation they provide a useful way to refer to for example the set of messages received in the protocol layer or further up.

## Conclusions
Overall a very interesting paper that presents practical approaches to providing fully verified systems.

As an overarching design though the idea seems to be to reduce the trust component from a multiple thousand lines of code codebase, to ~80 lines of the spec. Thus a consumer just has to trust/verify that the spec is what they desire and then they can have complete trust in the system.

There does seem to be some tradeoff in terms of performance, however that could also be attributed to the quantity of development time difference between systems.

Additionally their plot of throughput against latency is very nice, especially the interpolation between points, showing the characteristic curve of throughput initially descreasing after its peak due to contention rising.
