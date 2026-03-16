{-# LANGUAGE ScopedTypeVariables #-}

-- | Exception catching utilities, inlined from @unliftio@ and
-- @safe-exceptions@ to avoid the @exceptions@ transitive dependency.
--
-- All catching functions only catch synchronous exceptions by default.
module Control.Exception.Fault.Catch
  ( -- * Pure catching
    pureTry,
    pureTryDeep,
    -- * Throwing
    throwIO,
    throwTo,
    -- * Catching
    catch,
    catchJust,
    try,
    tryJust,
    tryAny,
    handle,
    -- * Masking
    mask,
    uninterruptibleMask,
    mask_,
    uninterruptibleMask_,
    -- * Cleanup
    bracket,
    bracket_,
    finally,
    onException,
    -- * Evaluation
    evaluate,
    -- * Sync\/async distinction
    SyncExceptionWrapper (..),
    AsyncExceptionWrapper (..),
    toSyncException,
    toAsyncException,
    isSyncException,
    isAsyncException,
    -- * Re-exports
    Exception (..),
    SomeException (..),
    SomeAsyncException (..),
    MonadIO (..),
    MonadUnliftIO (..),
  )
where

import Control.DeepSeq (NFData(rnf))
import Control.Concurrent (ThreadId)
import Control.Exception (Exception (..), SomeAsyncException (..), SomeException (..))
import qualified Control.Exception as E
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO (..))
import Data.Maybe (maybe)
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
-- Throwing
---------------------------------------------------------------------

-- | Throw an exception in 'MonadIO'.
throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . E.throwIO
{-# INLINE throwIO #-}

-- | Throw an exception to a thread.
throwTo :: MonadIO m => ThreadId -> SomeException -> m ()
throwTo tid = liftIO . E.throwTo tid
{-# INLINE throwTo #-}

---------------------------------------------------------------------
-- Catching (sync-only by default)
---------------------------------------------------------------------

-- | Catch synchronous exceptions matching a predicate.
catchJust :: (MonadUnliftIO m, Exception e)
          => (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchJust f action handler = catch action $ \e ->
  case f e of
    Just b  -> handler b
    Nothing -> throwIO e
{-# INLINE catchJust #-}

-- | Try an action, catching a specific synchronous exception.
try :: (MonadUnliftIO m, Exception e) => m a -> m (Either e a)
try action = catch (fmap Right action) (return . Left)
{-# INLINE try #-}

-- | Try an action, catching synchronous exceptions matching a predicate.
tryJust :: (MonadUnliftIO m, Exception e)
        => (e -> Maybe b) -> m a -> m (Either b a)
tryJust f action = catchJust f (fmap Right action) (return . Left)
{-# INLINE tryJust #-}

-- | Flipped 'catch'.
handle :: (MonadUnliftIO m, Exception e) => (e -> m a) -> m a -> m a
handle = flip catch
{-# INLINE handle #-}

---------------------------------------------------------------------
-- Masking
---------------------------------------------------------------------

-- | Lifted 'E.mask'.
mask :: MonadUnliftIO m => ((forall a. m a -> m a) -> m b) -> m b
mask f = withRunInIO $ \run ->
  E.mask $ \restore ->
    run (f (liftIO . restore . run))
{-# INLINE mask #-}

-- | Lifted 'E.uninterruptibleMask'.
uninterruptibleMask :: MonadUnliftIO m => ((forall a. m a -> m a) -> m b) -> m b
uninterruptibleMask f = withRunInIO $ \run ->
  E.uninterruptibleMask $ \restore ->
    run (f (liftIO . restore . run))
{-# INLINE uninterruptibleMask #-}

-- | Lifted 'E.mask_'.
mask_ :: MonadUnliftIO m => m a -> m a
mask_ action = mask $ \_ -> action
{-# INLINE mask_ #-}

-- | Lifted 'E.uninterruptibleMask_'.
uninterruptibleMask_ :: MonadUnliftIO m => m a -> m a
uninterruptibleMask_ action = uninterruptibleMask $ \_ -> action
{-# INLINE uninterruptibleMask_ #-}

---------------------------------------------------------------------
-- Cleanup
---------------------------------------------------------------------

-- | Lifted 'E.bracket' (async-exception safe).
bracket :: MonadUnliftIO m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket acquire release action = withRunInIO $ \run ->
  E.bracket (run acquire) (run . release) (run . action)
{-# INLINE bracket #-}

-- | Like 'bracket' but discards the resource.
bracket_ :: MonadUnliftIO m => m a -> m b -> m c -> m c
bracket_ acquire release action = bracket acquire (const release) (const action)
{-# INLINE bracket_ #-}

-- | Lifted 'E.finally'.
finally :: MonadUnliftIO m => m a -> m b -> m a
finally action cleanup = withRunInIO $ \run ->
  E.finally (run action) (run cleanup)
{-# INLINE finally #-}

-- | Lifted 'E.onException'.
onException :: MonadUnliftIO m => m a -> m b -> m a
onException action cleanup = withRunInIO $ \run ->
  E.onException (run action) (run cleanup)
{-# INLINE onException #-}

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
