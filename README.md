# Fault

The Monad{Throw,Catch,Mask,Error} lineage tried to make exceptions polymorphic over the monad, but exceptions are fundamentally an IO concept — throwIO and catch are IO primitives, and pretending otherwise just creates leaky abstractions. Stick with the minimal thing that can polymorphize `mask` and `bracket`: MonadUnliftio. Fault takes the opposite approach from all of those — instead of abstracting over where exceptions happen, it gives you a pure algebra for composing what to do about them. The exceptions are still IO; the handlers are profunctors. That's a clean separation.


## Overview

This library intends to make it as easy as possible for you to preserve failure context, render it to a display-agnostic form, and finally serialize it beautifully to a variety of outputs -- a terminal, a window in your GUI application, a log file, etc

## Failure Conversions

There are two standard ways to handle failures -- one is treating the failure as a value, and returning it in a disjunction, like Either. The other is exceptions, which don't appear in the type and have non-local effects. In general, we prefer the Either approach, but sometimes we don't really have a choice.

throwLeft and throwIOLeft convert the disjunction into an exception. This should only be used when necessary -- e.g., your function has external constraints that make it impossible for an Either to cross the boundary.
ad-hoc exception creation

If a failure type was created by an external library, it may not provide an Exception instance, which means you can't throw it. We provide types both for turning "regular" types into Exceptions and for adding CallStacks to Exceptions.

## Composable Exceptions

The Fault type is fundamentally Either SomeException a -> b — it's a profunctor for computations that can receive exceptions. The Arrow/Category instances give you
  composition. The (<!>) operator gives you exception-specific overrides. decide gives you contravariant branching.

Strip away the Testable/Record/Report/Render testing infrastructure, and what you have is:

A composable exception handler — something like:

-- Build handlers by composition
handler :: Fault Request Response
handler = ignore processRequest
      <!> accept @TimeoutException (const defaultResponse)
      <!> accept @AuthException (const forbidden403)

-- Run it
runFault handler request

The pieces that already exist and fit this:
- Fault newtype + profunctor/arrow instances
- ignore — lift a pure function, re-throwing on exception
- accept — catch a specific exception type
- (<!>) — override for a specific exception
- decide — branch on input
- recover — handle the exception path
- runFault / runFault' / withFault — evaluate

What's missing to make this a standalone exceptions-composition library:
- Nice combinators for chaining handlers (the Arrow instance gives >>> but more ergonomic ones would help)
- Retry/backoff (compose a Fault with itself N times)
- Resource cleanup integration (bracket-like patterns through Fault)
- Logging/tracing the exception path
- Maybe a FaultT monad transformer?

This would pair naturally with profunctor-optics-exceptions — the optics select exceptions, the Fault type composes handlers for them.

What angle appeals to you? Minimal (just the Fault core as a standalone profunctor, no testing), or something more like an ergonomic exception-handling DSL?


- NotInIO — type family that gives a compile error if you use throw in IO (forces throwIO)
- CallStacked — attach call stacks to exceptions
- ExceptFailure — throw any type as an exception without requiring Show
- Display — wrapper that makes GHC use displayException instead of show at top level
- throwLeft/throwIOLeft — eliminate Either by throwing

And Show.hs is a clean precedence-aware ShowS helper used by Throw.



