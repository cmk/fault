❯ id like to explore a few viable subsets. just spitballing here, but what about instead of a testing library we do a composable exceptions library of some sort?                                     

⏺ Interesting direction. The Fault type is fundamentally Either SomeException a -> b — it's a profunctor for computations that can receive exceptions. The Arrow/Category instances give you
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

❯ those are great ideas. i dropped two more modules into the tree, ptal and lmk wdyt. you'll need transformers to compile i think                                                                     

⏺ Searched for 1 pattern, read 2 files (ctrl+o to expand)

⏺ These are great. Throw.hs is a really nice composable exceptions module:

  - NotInIO — type family that gives a compile error if you use throw in IO (forces throwIO)
  - CallStacked — attach call stacks to exceptions
  - ExceptFailure — throw any type as an exception without requiring Show
  - Display — wrapper that makes GHC use displayException instead of show at top level
  - throwLeft/throwIOLeft — eliminate Either by throwing

  And Show.hs is a clean precedence-aware ShowS helper used by Throw.

  These fit perfectly with the Fault profunctor. Together they form a coherent "composable exceptions" library without any testing infrastructure. Let me get it compiling.
