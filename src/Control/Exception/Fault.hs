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

    Fault(..),
    ignore,
    accept,
    handle,
    fallback,
    handleAll,
    handleDefect,
    retry,
    retryWhen,

    -- * Evaluation
    recover,
    runFault,
    runFault',
    evaluate,
    
    -- * Running handlers in IO
    withFault,
    withFaultIO,
    withFaultMasked,
    withFaultBracket,

    -- * Combinators
    (<!>),
    decide,
    choice,
    refault,

    -- * Tracing
    trace,
    annotate,
    displayExceptions,

    -- * Catching and cleanup
    -- | See "Control.Exception.Fault.Catch" for the full API.
    try,
    tryAny,
    catch,
    bracket,
    finally,
    onException,
    
    -- * Throwing
    -- | See "Control.Exception.Fault.Throw" for the full API.
    throwIO,
    throwDefect,
    throwIODefect,
    throwLeft,
    throwIOLeft,
    NotInIO,
    
    -- * Exception wrappers
    -- | See "Control.Exception.Fault.Wrap" for the full API.
    module Control.Exception.Fault.Wrap,
    -- * Classes and common types
    -- | See "Control.Exception.Fault.Class" for the full API.
    module Control.Exception.Fault.Class,
) where

import Control.Exception.Fault.Class
import Control.Exception.Fault.Type
import Control.Exception.Fault.Wrap
import Control.Exception.Fault.Throw
  ( throwIO, throwDefect, throwIODefect, throwLeft, throwIOLeft
  , NotInIO, displayExceptions
  )
import Control.Exception.Fault.Catch
  ( withFaultIO, withFault, withFaultMasked, withFaultBracket
  , catch, try, tryAny
  , bracket, finally, onException
  , evaluate
  )
