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

= Modules

* "Control.Exception.Fault.Type" — The 'Fault' profunctor, instances, and combinators.
* "Control.Exception.Fault.Catch" — Lifted catch\/try\/bracket\/mask (sync-safe, no @exceptions@ dep).
* "Control.Exception.Fault.Throw" — Type-safe throw, 'CallStacked', 'Display', 'NotInIO'.

This module re-exports the full API.
-}
module Control.Exception.Fault (
    -- * Fault type and combinators
    module Control.Exception.Fault.Type,
    -- * Catching and cleanup
    module Control.Exception.Fault.Catch,
    -- * Throwing
    module Control.Exception.Fault.Throw,
) where

import Control.Exception.Fault.Type
import Control.Exception.Fault.Catch hiding
  ( handle       -- conflicts with Fault.Type.handle
  , evaluate     -- re-exported from Type
  , MonadIO(..)  -- re-exported from Type
  , MonadUnliftIO(..) -- re-exported from Type
  , Exception(..)     -- re-exported from Type
  , SomeException(..) -- re-exported from Type
  , SomeAsyncException(..)
  )
import Control.Exception.Fault.Throw hiding
  ( throw        -- conflicts with Fault.Type (internal)
  , Exception(..)
  , HasCallStack
  , MonadIO(..)
  , MonadUnliftIO(..)
  )
