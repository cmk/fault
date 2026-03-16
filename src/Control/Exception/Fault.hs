{- | Composable exception handlers.

A 'Fault' is a profunctor @Either SomeException a -> b@ — a computation
that receives a value or an exception and produces a result. The 'Category',
'Arrow', and profunctor instances give you composition, and '(<!>)' gives
you exception-specific overrides.

@
handler :: Fault Request Response
handler = ignore processRequest
      \<!\> handle \@TimeoutException (\\_ -> defaultResponse)
      \<!\> handle \@AuthException (\\_ -> forbidden403)

runFault handler request
@

== Running handlers in IO

Use 'withFaultIO' or 'withFault' to run a pure handler on a monadic action:

@
withFaultIO handler (readFile "config.yaml")
@

== Modules

* "Control.Exception.Fault.Type" — 'Fault' and 'Defect' types, construction, composition, retry, tracing.
* "Control.Exception.Fault.Wrap" — Exception wrappers: 'AnnotatedException', 'CallStackException', 'DisplayedException', sync\/async.
* "Control.Exception.Fault.Throw" — Type-safe throw ('NotInIO'), 'throwDefect', 'displayExceptions'.
* "Control.Exception.Fault.Catch" — Lifted catch\/try\/bracket\/mask (sync-safe, no @exceptions@ dep).
* "Control.Exception.Fault.Class" — Re-exports: 'Exception', 'MonadIO', 'MonadUnliftIO', 'HasCallStack', 'NFData'.
-}
module Control.Exception.Fault (
    -- * Running handlers in IO
    withFaultIO,
    withFault,
    -- * Fault type and combinators
    module Control.Exception.Fault.Type,
    -- * Exception wrappers
    module Control.Exception.Fault.Wrap,
    -- * Throwing
    module Control.Exception.Fault.Throw,
    -- * Catching and cleanup
    module Control.Exception.Fault.Catch,
    -- * Classes and common types
    module Control.Exception.Fault.Class,
) where

import Control.Exception.Fault.Class
import Control.Exception.Fault.Type
import Control.Exception.Fault.Catch hiding (handle, throwIO)
import Control.Exception.Fault.Throw hiding (throw)
import Control.Exception.Fault.Wrap
