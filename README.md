[![Haddocks](https://img.shields.io/badge/docs-haddocks-blue)](https://cmk.github.io/fault/)
[![CI](https://github.com/cmk/fault/actions/workflows/ci.yml/badge.svg)](https://github.com/cmk/fault/actions/workflows/ci.yml)

# fault

Composable exception handlers as profunctors.

Exceptions are IO primitives; handlers are pure. `Fault` gives you
an algebra for composing *what to do about* exceptions without
abstracting over *where* they happen.

## The Fault type

### Theory

A `Fault a b` is `Either SomeException a -> b` — a function from
"a value or an exception" to a result. This is a profunctor
(`Costar (Either SomeException)`), giving it `Category`, `Arrow`,
`ArrowChoice`, `Profunctor`, `Strong`, and `Choice` instances.

Handlers compose left-to-right with `(>>>)` from `Category`:

```haskell
newtype Fault a b = Fault { unFault :: Either SomeException a -> b }
```

The `(<!>)` operator overrides handling for specific exception
types, using the exception hierarchy via `fromException`/`toException`.

### Example 1: basic handler composition

```haskell
import Control.Exception.Fault

handler :: Fault Request Response
handler = ignore processRequest
      <!> accept @TimeoutException (\_ -> defaultResponse)
      <!> accept @AuthException    (\_ -> forbidden403)

-- Runs processRequest on the input.
-- If a TimeoutException was thrown, returns defaultResponse.
-- If an AuthException was thrown, returns forbidden403.
-- All other exceptions are ignored (passed through processRequest).
result = runFault handler request
```

### Example 2: pipeline with retry

```haskell
import Control.Exception.Fault

-- Retry up to 3 times, then process the result:
pipeline :: Fault RawData ProcessedData
pipeline = retry 3 >>> ignore processResult

-- Compose with tracing:
traced :: Fault RawData ProcessedData
traced = trace "step1" (retry 3) >>> trace "step2" (ignore processResult)
```

## Handler construction

### Theory

Handlers are built from a small set of combinators that each
express a different error-handling strategy:

| Combinator | Strategy |
|---|---|
| `ignore f` | Apply `f`, ignore exceptions |
| `accept @E f` | Handle exception type `E` with `f` |
| `handle f` | Handle any exception with `f` |
| `handleAll f` | Handle with access to `SomeException` |
| `fallback b` | Return a default value on any exception |
| `decide` | Return `Left exception` or `Right value` |

### Example 1: fallback with default

```haskell
import Control.Exception.Fault

-- Return 0 if anything goes wrong:
>>> runFault (fallback 0) (error "boom" :: Int)
0
>>> runFault (fallback 0) 42
42
```

### Example 2: decide + choice

```haskell
import Control.Exception.Fault

-- Inspect the exception:
>>> runFault decide (error "boom" :: Int)
Left (SomeException ...)
>>> runFault decide 42
Right 42

-- Route based on exception presence:
>>> runFault (choice (const "failed") show) (error "boom" :: Int)
"failed"
>>> runFault (choice (const "failed") show) 42
"42"
```

## Override with (<!>)

### Theory

`(<!>)` composes two `Fault` handlers with exception-type-specific
override. The right-hand handler takes priority for its declared
exception type, falling through to the left-hand handler otherwise.

This gives you a clean pattern for building layered exception
handlers:

```haskell
(<!>) :: Exception e => Fault a b -> (e -> a -> b) -> Fault a b
```

### Example 1: type-specific override

```haskell
import Control.Exception.Fault
import Control.Exception (IOException)

handler :: Fault String String
handler = ignore id
      <!> accept @IOException (\_ -> "IO error caught")

>>> runFault handler "hello"
"hello"
-- If an IOException was thrown: "IO error caught"
```

### Example 2: layered handlers

```haskell
import Control.Exception.Fault

handler :: Fault Input Output
handler = ignore normalProcess
      <!> accept @TimeoutException  (\_ -> timeoutResponse)
      <!> accept @AuthException     (\_ -> authFailResponse)
      <!> accept @RateLimitException (\e -> retryAfter (retryDelay e))
```

## Retry

### Theory

`retry n` re-evaluates the input up to `n` times on any synchronous
exception. `retryWhen n p` adds a predicate — only retry if `p`
returns `True` for the exception. Both compose as `Fault` values.

### Example 1: simple retry

```haskell
import Control.Exception.Fault

-- Retry up to 3 times:
>>> withFault (retry 3) riskyIOAction
```

### Example 2: conditional retry

```haskell
import Control.Exception.Fault

-- Only retry on transient errors:
>>> withFault (retryWhen 3 isTransient) databaseQuery

-- Compose: retry then process
>>> withFault (retry 3 >>> ignore processResult) fetchAndProcess
```

## Type-safe throw

### Theory

GHC's `throw` works in any type, including pure code and `IO`.
This is problematic: throwing in `IO` should use `throwIO` (which
respects exception ordering), but the types don't enforce it.

fault's `throw` uses a `NotInIO` constraint to reject `throw`
in `IO` contexts at compile time:

```haskell
throw :: (NotInIO m, Exception e) => e -> m a
```

### Example 1: pure throw (compiles)

```haskell
import Control.Exception.Fault.Throw

lenient :: String -> Int
lenient = throw (userError "bang")
-- Compiles: String -> Int is not IO
```

### Example 2: IO throw (rejected)

```haskell
-- Does not compile:
-- strict :: IO Int
-- strict = throw (userError "bang")
--   error: Can't use `throw` in `IO Int`.
--          Try using `throwIO` instead.
```

## Call stack attachment

### Theory

Haskell exceptions don't carry call stacks by default.
`throwWithCallStack` wraps any exception in a `CallStackException`
that records `HasCallStack` information at the throw site.
`catchIgnoringStack` / `handleIgnoringStack` unwrap transparently.

### Example 1: throw with call stack

```haskell
import Control.Exception.Fault.Throw

myFunction :: HasCallStack => IO ()
myFunction = throwIOWithCallStack (userError "something went wrong")
-- Exception will carry the call stack from this point
```

### Example 2: displayExceptions for main

```haskell
import Control.Exception.Fault.Throw

-- GHC uses `show` for uncaught exceptions, which is ugly.
-- Wrap main to use `displayException` instead:
main :: IO ()
main = displayExceptions $ do
    ...
```

## Defect (throw arbitrary types)

### Theory

`Defect` wraps any `Typeable` value as an exception, with a
user-supplied display function. This lets you throw domain-specific
error types without defining `Exception` instances for each one.

### Example 1: throw a defect

```haskell
import Control.Exception.Fault.Throw

data MyError = MyError String deriving (Show, Typeable)

>>> throwDefect show (MyError "oops")
*** Exception: Defect: MyError "oops"
```

### Example 2: handle defects specifically

```haskell
import Control.Exception.Fault

handler :: Fault Input Output
handler = ignore normalProcess
      <!> handleDefect (\val -> errorResponse (show val))
```

## Modules

| Module | Purpose |
|---|---|
| `Control.Exception.Fault` | Re-export hub |
| `Control.Exception.Fault.Type` | `Fault` type, construction, composition, retry, evaluation |
| `Control.Exception.Fault.Catch` | Sync-only catch/try/bracket, lifted to `MonadUnliftIO` |
| `Control.Exception.Fault.Throw` | Type-safe `throw` (`NotInIO`), call stacks, display fix |
| `Control.Exception.Fault.Wrap` | Exception wrappers: `Defect`, `CallStackException`, `DisplayedException`, sync/async |
| `Control.Exception.Fault.Class` | Re-exports from `Control.Exception` and `unliftio-core` |

## Dependencies

```
base, deepseq, distributive, profunctors, transformers, unliftio-core
```

No dependency on `exceptions` or `safe-exceptions`.
