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
    Display (..),
    -- * Sync\/async conversion
    SyncExceptionWrapper (..),
    AsyncExceptionWrapper (..),
  )
where

import Control.Exception.Fault.Class
import Control.Exception.Fault.Type (Defect(..), defect)
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
-- Exception instance for Defect e String
---------------------------------------------------------------------

-- | @'Defect' e String@ is throwable: the projection @e -> String@ provides
-- 'displayException', avoiding the need for a 'Show' instance on @e@.
--
-- @
-- throwIO (Defect myValue show)
-- @
instance Typeable e => Show (Defect e String) where
  showsPrec p d =
    showParen (p > 10) $
      showString "Defect _ " . showsPrec 11 (defect d)

instance Typeable e => Exception (Defect e String) where
  displayException = defect

---------------------------------------------------------------------
-- Display
---------------------------------------------------------------------

-- | A wrapper that makes GHC use 'displayException' instead of 'show'
-- when printing uncaught exceptions.
--
-- @
-- main = 'Control.Exception.Fault.Throw.displayExceptions' actualMain
-- @
--
-- See [Why doesn't GHC use my displayException method?](https://stackoverflow.com/questions/55490766)
newtype Display a = Display a

instance Exception e => Show (Display e) where
  show (Display e) = displayException e

instance Exception e => Exception (Display e)

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
