# Fault

Composable exception handlers as profunctors.

Exceptions are IO primitives; handlers are pure. `Fault` gives you an algebra for composing *what to do about* exceptions without abstracting over *where* they happen.

## Usage

### Composing handlers

```haskell
import Control.Exception.Fault

handler :: Fault Request Response
handler = ignore processRequest
      <!> handle @TimeoutException (\_ -> defaultResponse)
      <!> handle @AuthException    (\_ -> forbidden403)

result = runFault handler request
```

### Retry

```haskell
-- Retry up to 3 times on any synchronous exception
runFault (retry 3) riskyComputation

-- Retry with a predicate
runFault (retryWhen 3 isTransient) riskyComputation

-- Compose with downstream processing
retry 3 >>> ignore processResult
```

### Type-safe throw (`NotInIO`)

```haskell
-- Compiles: pure context
lenient :: String -> Int
lenient = throw (userError "bang")

-- Does not compile: IO context
-- strict :: IO Int
-- strict = throw (userError "bang")
--   error: Can't use a `throw` function in `IO Int`.
--          Try using the `throwIO` variant instead.
```

### `displayExceptions` for main

GHC prints uncaught exceptions with `show`, not `displayException`.
Wrap your `main` to fix this:

```haskell
main :: IO ()
main = displayExceptions $ do
  ...
```

### `Defect` -- throw arbitrary types

```haskell
-- Throw any Typeable value, providing a display function
throwDefect show myValue

-- In MonadIO
throwIODefect show myValue
```

## Modules

| Module | Purpose |
|---|---|
| `Control.Exception.Fault` | Re-export hub |
| `Control.Exception.Fault.Type` | `Fault` type, construction, composition, retry, evaluation |
| `Control.Exception.Fault.Catch` | Sync-only catch/try/bracket, lifted to `MonadUnliftIO` |
| `Control.Exception.Fault.Throw` | Type-safe `throw` (`NotInIO`), call stacks, display fix |
| `Control.Exception.Fault.Wrap` | Exception wrappers: `Defect`, `CallStackException`, `Display`, sync/async |
| `Control.Exception.Fault.Class` | Re-exports from `Control.Exception` and `unliftio-core` |

## Dependencies

`base`, `deepseq`, `profunctors`, `transformers`, `unliftio-core`

No dependency on `exceptions` or `safe-exceptions`.
