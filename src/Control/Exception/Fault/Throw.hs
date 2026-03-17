{-# language DataKinds
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances #-}

-- | Type-safe throwing, call stack attachment, and display fixes.
--
-- This module provides:
--
-- * A 'throw' that gives a compile error if used in 'IO' ('NotInIO')
-- * 'MonadIO'-lifted variants of all throwing functions
-- * 'MonadUnliftIO'-lifted catch\/handle that see through wrappers
module Control.Exception.Fault.Throw
  ( -- * Pure throw (with NotInIO guard)
    throw,
    throwDefect,
    throwWithCallStack,
    throwLeft,
    -- * Monadic throw
    throwIO,
    throwIOLeft,
    throwIODefect,
    throwIOWithCallStack,
    -- * Call stacks
    CallStackException (..),
    addCallStack,
    catchIgnoringStack,
    handleIgnoringStack,
    -- * Display fix
    DisplayedException (..),
    displayExceptions,
    catchIgnoringDisplay,
    handleIgnoringDisplay,
    -- * Throw checking
    NotInIO,
  )
where

import           Control.Exception.Fault.Class
import           Control.Exception.Fault.Type (Defect(..))
import           Control.Exception.Fault.Wrap (CallStackException(..), DisplayedException(..))
import qualified Control.Exception.Fault.Catch as Catch
import qualified Control.Exception as Except
import           Control.Monad.Trans.Accum
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.RWS.CPS as CPS
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Select
import qualified Control.Monad.Trans.State.Lazy as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import qualified Control.Monad.Trans.Writer.CPS as CPS
import qualified Control.Monad.Trans.Writer.Lazy as Lazy
import qualified Control.Monad.Trans.Writer.Strict as Strict
import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage(..), TypeError)

---------------------------------------------------------------------
-- Pure throw (with NotInIO guard)
---------------------------------------------------------------------

-- | A replacement for @Control.Exception.'Except.throw'@ that gives a
-- compile error if used in 'IO'. Use 'throwIO' in monadic code instead.
throw :: Exception e => NotInIO a => e -> a
throw = Except.throw

-- | Throw any 'Typeable' value as a 'Defect'. The function provides
-- 'displayException' if uncaught.
throwDefect :: Typeable e => NotInIO a => (e -> String) -> e -> a
throwDefect f e = throw (Defect e f)

-- | Throw an exception with the current 'CallStack' attached.
throwWithCallStack :: HasCallStack => Exception e => NotInIO a => (CallStack -> e) -> a
throwWithCallStack = throw . ($ callStack)

-- | Eliminate an 'Either' by throwing the 'Left'.
throwLeft :: Exception e => NotInIO a => Either e a -> a
throwLeft = either throw id

---------------------------------------------------------------------
-- Monadic throw
---------------------------------------------------------------------

-- | Throw an exception in any 'MonadIO'.
--
-- In general it's preferable to treat a failure as a value in a disjunction
-- (e.g. `Left` in `Either`) rather than as an exception, which doesn't
-- appear in the type and has non-local effects.
--
-- However if you have no choice then use `throwIO` and/or `throwIOLeft`
-- to convert the disjunction into an exception.
--
throwIO :: MonadIO m => Exception e => e -> m a
throwIO = Catch.throwIO

-- | Eliminate an 'Either' by throwing the 'Left' in 'MonadIO'.
throwIOLeft :: MonadIO m => Exception e => Either e a -> m a
throwIOLeft = either throwIO pure

-- | Throw any 'Typeable' value as a 'Defect' in 'MonadIO'.
throwIODefect :: MonadIO m => Typeable e => (e -> String) -> e -> m a
throwIODefect f e = throwIO (Defect e f)

-- | Throw an exception with the current 'CallStack' in 'MonadIO'.
throwIOWithCallStack :: HasCallStack => MonadIO m => Exception e => (CallStack -> e) -> m a
throwIOWithCallStack = throwIO . ($ callStack)

---------------------------------------------------------------------
-- Call stacks
---------------------------------------------------------------------

-- | Attach the current 'CallStack' to an exception.
addCallStack :: HasCallStack => e -> CallStackException e
addCallStack e = CallStackException e callStack

-- | Catch an exception, seeing through 'CallStackException' wrappers.
--
-- __NB__: If you re-throw, the original 'CallStack' is discarded.
catchIgnoringStack :: MonadUnliftIO m => Exception e => m a -> (e -> m a) -> m a
catchIgnoringStack = flip handleIgnoringStack

-- | Handle an exception, seeing through 'CallStackException' wrappers.
handleIgnoringStack :: MonadUnliftIO m => Exception e => (e -> m a) -> m a -> m a
handleIgnoringStack f = Catch.handle f . Catch.handle (\(CallStackException e _) -> f e)

---------------------------------------------------------------------
-- DisplayedException fix
---------------------------------------------------------------------

-- | Fix @main@ to use 'displayException' instead of 'show' for uncaught exceptions.
--
-- @
-- main = displayExceptions actualMain
-- @
--
-- See [Why doesn't GHC use my displayException method?](https://stackoverflow.com/questions/55490766)
displayExceptions :: MonadUnliftIO m => m a -> m a
displayExceptions =
  handleIgnoringDisplay (\e -> throwIO (DisplayedException (e :: SomeException)))

-- | Handle an exception, seeing through 'DisplayedException' wrappers.
handleIgnoringDisplay :: MonadUnliftIO m => Exception e => (e -> m a) -> m a -> m a
handleIgnoringDisplay f = Catch.handle f . Catch.handle (\(DisplayedException e) -> f e)

-- | Catch variant of 'handleIgnoringDisplay'.
catchIgnoringDisplay :: MonadUnliftIO m => Exception e => m a -> (e -> m a) -> m a
catchIgnoringDisplay = flip handleIgnoringDisplay

---------------------------------------------------------------------
-- NotInIO
---------------------------------------------------------------------

-- | Constraint that gives a compile error if the return type is @IO a@
-- or any transformer stack over @IO@. This prevents accidental use of
-- imprecise 'throw' where 'throwIO' is needed.
type family NotInIO a :: Constraint where
  NotInIO (IO a) =
    TypeError
    ('Text "Can't use a `throw` function in `" ':<>:
     ShowType (IO a) ':<>:
     'Text "`." ':$$:
     'Text "Try using the `throwIO` variant instead.")
  NotInIO (AccumT w m a) = NotInIO (m (a, w))
  NotInIO (ContT r m _) = NotInIO (m r)
  NotInIO (ExceptT e m a) = NotInIO (m (Either e a))
  NotInIO (IdentityT m a) = NotInIO (m a)
  NotInIO (MaybeT m a) = NotInIO (m (Maybe a))
  NotInIO (ReaderT _ m a) = NotInIO (m a)
  NotInIO (SelectT _ m a) = NotInIO (m a)
  NotInIO (CPS.RWST _ w s m a) = NotInIO (m (a, s, w))
  NotInIO (CPS.WriterT w m a) = NotInIO (m (a, w))
  NotInIO (Lazy.RWST _ w s m a) = NotInIO (m (a, s, w))
  NotInIO (Lazy.StateT s m a) = NotInIO (m (a, s))
  NotInIO (Lazy.WriterT w m a) = NotInIO (m (a, w))
  NotInIO (Strict.RWST _ w s m a) = NotInIO (m (a, s, w))
  NotInIO (Strict.StateT s m a) = NotInIO (m (a, s))
  NotInIO (Strict.WriterT w m a) = NotInIO (m (a, w))
  NotInIO _ = ()
