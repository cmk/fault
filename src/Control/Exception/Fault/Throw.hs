{-# language DataKinds
           , TypeFamilies
           , TypeOperators
           , UndecidableInstances #-}

-- | Various tools for improving the quality of the failures in your programs.
--   This is meant to encourage you to "do the right thing" more often.
module Control.Exception.Fault.Throw
  ( -- * A better `Except.throw`
    throw,
    -- * Call stacks
    CallStackException (..),
    catchIgnoringStack,
    handleIgnoringStack,
    addCallStack,
    throwWithCallStack,
    throwIOWithCallStack,
    -- * Lifting values to exceptions
    Defect (..),
    throwDefect,
    throwIODefect,
    -- * Disjunctions to exceptions
    throwLeft,
    throwIOLeft,
    -- * Display fix
    Display (..),
    displayExceptions,
    catchIgnoringDisplay,
    handleIgnoringDisplay,
    -- * Throw checking
    NotInIO,
  )
where

import           Control.Exception.Fault.Class
import           Control.Exception.Fault.Wrap (CallStackException(..), Defect(..), Display(..))
import           Control.Exception (catch, handle, throwIO)
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
import GHC.Stack (prettyCallStack)
import GHC.TypeLits (ErrorMessage(..), TypeError)
import System.Exit (exitFailure)
import System.IO (IO, hPutStr, stderr)


-- | A replacement for `Except.throw` that complains if you should be calling
--  `throwIO` instead.
throw :: (Exception e, NotInIO a) => e -> a
throw = Except.throw

-- | See `handleIgnoringStack`.
catchIgnoringStack :: Exception e => IO a -> (e -> IO a) -> IO a
catchIgnoringStack = flip handleIgnoringStack

-- | Like `handle`, but checks for both the "bare" and 'CallStackException'
--   versions of an exception.
--
--  __NB__: This does mean that if you re-`throwIO` you'll discard the original
--         `CallStack`.
handleIgnoringStack :: Exception e => (e -> IO a) -> IO a -> IO a
handleIgnoringStack f = handle f . handle (\(CallStackException e _) -> f e)

-- | Adds the current call stack to the exception.
addCallStack :: HasCallStack => e -> CallStackException e
addCallStack e = CallStackException e callStack

-- | Tries to prevent using non-`IO`-safe `throw` operations in `IO`. It covers
--   the standard transformers, but still can't /guarantee/ that there's no `IO`
--   in the stack, in which case the constraint name at least gives an extra
--   hint to callers. The constraint may need to be propagated and it also tries
--   to give a helpful error message when it fails.
type family NotInIO a :: Constraint where
  -- The bulk of the cases are the long way of saying
  -- @`MonadIO` m => m a = `TypeError` ...@ but without being able to catch any
  -- instances we don't know about a priori.
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
  -- and otherwise we're safe ... relatively
  NotInIO _ = ()

-- | Throws any type. The provided function is what's used for serialization if
--   the exception isn't caught.
throwDefect :: (Typeable e, NotInIO a) => (e -> String) -> e -> a
throwDefect f = throw . Defect f

-- | See 'throwDefect'.
throwIODefect :: Typeable e => (e -> String) -> e -> IO a
throwIODefect f = throwIO . Defect f

-- | Throws an exception that's expecting to carry a `CallStack`.
--
--   I /think/ these two should be equivalent
--
-- > throwWithCallStack . CallStackException
-- > throw . addCallStack
throwWithCallStack :: (HasCallStack, Exception e, NotInIO a) => (CallStack -> e) -> a
throwWithCallStack = throw . ($ callStack)

-- | See `throwWithCallStack`.
throwIOWithCallStack :: (HasCallStack, Exception e) => (CallStack -> e) -> IO a
throwIOWithCallStack = throwIO . ($ callStack)

-- | Eliminate an `Either` by throwing the left.
throwLeft :: (Exception e, NotInIO a) => Either e a -> a
throwLeft = either throw id

-- | See `throwLeft`.
throwIOLeft :: Exception e => Either e a -> IO a
throwIOLeft = either throwIO pure

-- | This "fixes" @main@ to use `displayException` instead of `show` when
--   failing with an exception.
--
--   E.g., replace @main = x@ with @main = displayExceptions $ x@. This is also
--   useful before `unsafePerformIO` if you won't get control back again (like,
--   in a GHC plugin).
--
--   This works by wrapping the `Exception` in a newtype that has the correct
--   behavior, which means that if you want to try handling these exceptions
--   outside a call to `displayExceptions`, you should be using
--  `handleIgnoringDisplay` (or `catchIgnoringDisplay`).
--
--   See [Why doesn’t GHC use my `displayException`
--   method?](https://stackoverflow.com/questions/55490766/why-doesn-t-ghc-use-my-displayexception-method)
--   for some explanation. I find it totally unconvincing, and I think Kmett's
--   comment about "\'helpful\' `Show` instances" makes the argument /for/ using
--  `displayException` -- with the current behavior, users are encouraged to
--   define a custom `show` to get GHC to output a useful failure message, which
--   then breaks the /intended/ use of `show` as an syntax printer.
displayExceptions :: IO a -> IO a
displayExceptions =
  -- Using `handleIgnoringDisplay` in order to avoid re-wrapping already-wrapped
  -- exceptions.
  handleIgnoringDisplay (\e -> throwIO (Display (e :: SomeException)))

-- | Handles an exception that /may/ be wrapped in the `Display` @newytpe@,
--   unwrapping it if necessary.
handleIgnoringDisplay :: Exception e => (e -> IO a) -> IO a -> IO a
handleIgnoringDisplay f = handle f . handle (\(Display e) -> f e)

-- | See `handleIgnoringDisplay`.
catchIgnoringDisplay :: Exception e => IO a -> (e -> IO a) -> IO a
catchIgnoringDisplay = flip handleIgnoringDisplay
