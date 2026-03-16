{-# LANGUAGE ScopedTypeVariables #-}

-- | Exception catching utilities, inlined from @unliftio@ and
-- @safe-exceptions@ to avoid the @exceptions@ transitive dependency.
--
-- All catching functions only catch synchronous exceptions by default.
module Control.Exception.Fault.Catch
  ( -- * Pure catching
    pureTry,
    pureTryDeep,
    -- * IO catching
    catch,
    tryAny,
    evaluate,
    -- * Sync\/async distinction
    SyncExceptionWrapper (..),
    AsyncExceptionWrapper (..),
    toSyncException,
    toAsyncException,
    isSyncException,
    isAsyncException,
    -- * Re-exports
    MonadIO (..),
    MonadUnliftIO (..),
  )
where

import Control.DeepSeq (NFData(rnf))
import Control.Exception (Exception (..), SomeAsyncException (..), SomeException (..))
import qualified Control.Exception as E
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO (..))
import Data.Typeable (Typeable, cast)
import Prelude
import System.IO.Unsafe (unsafePerformIO)

-- | Try to evaluate a value purely, catching any impure exceptions.
pureTry :: a -> Either SomeException a
pureTry a = unsafePerformIO $
  (return $! Right $! a) `E.catch` \e -> return (Left (e :: SomeException))
{-# INLINE pureTry #-}

-- | Like 'pureTry', but fully evaluates the value via 'NFData' first.
pureTryDeep :: NFData a => a -> Either SomeException a
pureTryDeep a = unsafePerformIO $
  (Right <$> E.evaluate (force a)) `E.catch` \e -> return (Left (e :: SomeException))
  where force x = rnf x `seq` x
{-# INLINE pureTryDeep #-}

-- | Catch synchronous exceptions only (async exceptions are re-thrown).
catch :: (MonadUnliftIO m, Exception e) => m a -> (e -> m a) -> m a
catch action handler = withRunInIO $ \run ->
  run action `E.catch` \e ->
    if isSyncException e
      then run (handler e)
      else E.throwIO e
{-# INLINE catch #-}

-- | Try an action, catching any synchronous exception.
tryAny :: MonadUnliftIO m => m a -> m (Either SomeException a)
tryAny action = catch (fmap Right action) (return . Left)
{-# INLINE tryAny #-}

-- | Evaluate to WHNF in the current monad.
evaluate :: MonadIO m => a -> m a
evaluate = liftIO . E.evaluate
{-# INLINE evaluate #-}

---------------------------------------------------------------------
-- Sync/async exception wrappers (from safe-exceptions)
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

-- | Convert an exception to a synchronous one (wrapping if async).
toSyncException :: Exception e => e -> SomeException
toSyncException e
  | isSyncException e = toException e
  | otherwise = toException (SyncExceptionWrapper (toException e))
{-# INLINE toSyncException #-}

-- | Convert an exception to an asynchronous one (wrapping if sync).
toAsyncException :: Exception e => e -> SomeException
toAsyncException e
  | isAsyncException e = toException e
  | otherwise = toException (AsyncExceptionWrapper (toException e))
{-# INLINE toAsyncException #-}

-- | Check if an exception is synchronous.
isSyncException :: Exception e => e -> Bool
isSyncException e =
  case fromException (toException e) of
    Just (SomeAsyncException _) -> False
    Nothing -> True
{-# INLINE isSyncException #-}

-- | Check if an exception is asynchronous.
isAsyncException :: Exception e => e -> Bool
isAsyncException = not . isSyncException
{-# INLINE isAsyncException #-}
