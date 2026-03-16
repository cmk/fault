{-# LANGUAGE ExistentialQuantification #-}

-- | Exception wrappers for enriching, converting, and lifting exceptions.
--
-- These newtypes wrap exceptions to add context (call stacks, annotations),
-- convert between sync\/async, lift arbitrary types into the exception
-- hierarchy, or fix GHC's display behavior.
module Control.Exception.Fault.Wrap
  ( -- * Context
    AnnotatedException (..),
    CallStackException (..),
    -- * Lifting
    Defect (..),
    -- * Display
    Display (..),
    -- * Sync\/async conversion
    SyncExceptionWrapper (..),
    AsyncExceptionWrapper (..),
  )
where

import Control.Exception.Fault.Class
import Data.Typeable (cast)
import GHC.Stack (prettyCallStack)

---------------------------------------------------------------------
-- Context
---------------------------------------------------------------------

-- | An exception annotated with a message string.
--
-- Used by 'Control.Exception.Fault.Type.annotate' to add context
-- as exceptions propagate through composed handlers.
data AnnotatedException = AnnotatedException String SomeException
  deriving (Show, Typeable)

instance Exception AnnotatedException where
  displayException (AnnotatedException msg e) =
    msg ++ ": " ++ displayException e

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
-- Lifting
---------------------------------------------------------------------

-- | Lift any 'Typeable' value into the exception hierarchy.
--
-- The provided function is used for 'displayException' if the
-- exception is not caught. This avoids requiring a 'Show' instance
-- on the error type.
--
-- @
-- 'throwIO' ('Defect' show myValue)
-- @
--
-- In IEEE 1044 terminology, a defect is a concrete manifestation
-- of an error in software — an imperfection that may lead to a fault.
data Defect e = Defect
  { defectDisplay :: e -> String
  , defect :: e
  }

instance Show (Defect e) where
  showsPrec p (Defect f e) =
    showParen (p > 10) $
      showString "Defect " . showString "_" . showString " " . showsPrec 11 (f e)

instance Typeable e => Exception (Defect e) where
  displayException (Defect f e) = f e

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
