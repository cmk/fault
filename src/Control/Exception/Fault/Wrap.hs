{-# LANGUAGE ExistentialQuantification #-}

-- | Exception wrappers for enriching, converting, and lifting exceptions.
--
-- These newtypes wrap exceptions to add context (call stacks, annotations),
-- convert between sync\/async, lift arbitrary types into the exception
-- hierarchy, or fix GHC's display behavior.
module Control.Exception.Fault.Wrap
  ( -- * Context
    CallStackException (..),
    -- * Display
    DisplayedException (..),
    -- * Sync\/async conversion
    SyncExceptionWrapper (..),
    AsyncExceptionWrapper (..),
  )
where

import Control.Exception.Fault.Class
import Data.Typeable (cast)
import GHC.Stack (prettyCallStack)

---------------------------------------------------------------------
-- Context (AnnotatedException re-exported from Type)
---------------------------------------------------------------------

-- | An exception with an attached 'CallStack'.
--
-- @
-- 'throwIO' ('CallStackException' myException 'callStack')
-- @
data CallStackException e = CallStackException
  { stacked :: e
  , stackTrace :: CallStack
  }
  deriving Show

instance Exception e => Exception (CallStackException e) where
  displayException (CallStackException e cs) =
    displayException e ++ "\n" ++ prettyCallStack cs

---------------------------------------------------------------------
-- DisplayedException
---------------------------------------------------------------------

-- | A zero-overhead wrapper that makes GHC use 'displayException'
-- instead of 'show' when printing uncaught exceptions.
--
-- Unlike 'Control.Exception.Fault.Type.AnnotatedException', this does
-- not add any information to the exception — it only fixes GHC's
-- rendering behavior. Use 'displayExceptions' to wrap @main@.
--
-- @
-- main = 'Control.Exception.Fault.Throw.displayExceptions' actualMain
-- @
--
-- See [Why doesn't GHC use my displayException method?](https://stackoverflow.com/questions/55490766)
newtype DisplayedException a = DisplayedException a

instance Exception e => Show (DisplayedException e) where
  show (DisplayedException e) = displayException e

instance Exception e => Exception (DisplayedException e)

---------------------------------------------------------------------
-- Sync/async conversion
---------------------------------------------------------------------

-- | Wrap a synchronous exception to be thrown asynchronously.
newtype AsyncExceptionWrapper = AsyncExceptionWrapper
  { unAsyncExceptionWrapper :: SomeException }
  deriving (Show, Typeable)

instance Exception AsyncExceptionWrapper where
  toException = toException . SomeAsyncException
  fromException se = do
    SomeAsyncException e <- fromException se
    cast e

-- | Wrap an asynchronous exception to be caught synchronously.
newtype SyncExceptionWrapper = SyncExceptionWrapper
  { unSyncExceptionWrapper :: SomeException }
  deriving (Show, Typeable)

instance Exception SyncExceptionWrapper
