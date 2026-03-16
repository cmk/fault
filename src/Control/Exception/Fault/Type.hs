{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- | Composable exception handlers.

A 'Fault' is a profunctor @Either SomeException a -> b@ — a computation
that receives a value or an exception and produces a result. The 'Category',
'Arrow', and profunctor instances give you composition, and '(<!>)' gives
you exception-specific overrides.

@
handler :: Fault Request Response
handler = ignore processRequest
      \<!\> accept \@TimeoutException (const defaultResponse)
      \<!\> accept \@AuthException (const forbidden403)

runFault handler request
@
-}
module Control.Exception.Fault.Type (

    -- * Fault
    Fault (..),

    -- * Defect
    Defect (..),
    defect,
    redefect,

    -- * Construction
    ignore,
    accept,
    handle,
    handleAll,
    fallback,

    -- * Selection
    (<!>),
    decide,
    choice,
    refault,

    -- * Retry
    retry,
    retryWhen,

    -- * Tracing
    trace,
    annotate,

    -- * Evaluation
    recover,
    runFault,
    runFault',
    withFaultIO,
    withFault,

) where

import Control.Applicative
import Control.Arrow (Arrow(..), ArrowChoice(..), ArrowLoop(..))
import Control.Category (Category)
import qualified Control.Category as C
import Control.Exception.Fault.Class
import Control.Exception.Fault.Catch (pureTry, pureTryDeep, evaluate)
import qualified Control.Exception.Fault.Catch as Catch
import qualified Control.Exception as Ex
import Control.Monad
import Data.Bifunctor (bimap)
import Data.Profunctor (Profunctor(..), Strong(..), Choice(..), Closed(..), Costrong(..), Cochoice(..))
import Data.Profunctor.Types (Costar(..))
import Prelude
import System.IO.Unsafe (unsafePerformIO)

-- | A software fault handler.
--
-- A @'Fault' a b@ receives either an exception or a value of type @a@,
-- and produces a @b@. The profunctor, 'Arrow', and 'Category' instances
-- allow composing handlers.
--
-- Adapted from the
-- < https://standards.ieee.org/standard/1044-2009.html IEEE Standard Classification for Software Anomalies >.
newtype Fault a b = Fault {unFault :: Either SomeException a -> b}
    deriving (Typeable)

deriving via (Costar (Either SomeException)) instance Closed Fault
deriving via (Costar (Either SomeException)) instance Costrong Fault
deriving via (Costar (Either SomeException)) instance Cochoice Fault
deriving via (Costar (Either SomeException)) instance Profunctor Fault
deriving via (Costar (Either SomeException) a) instance Functor (Fault a)
deriving via (Costar (Either SomeException) a) instance Applicative (Fault a)
deriving via (Costar (Either SomeException) a) instance Monad (Fault a)
deriving via ((->) (Either SomeException a) b) instance Semigroup b => Semigroup (Fault a b)
deriving via ((->) (Either SomeException a) b) instance Monoid b => Monoid (Fault a b)

instance C.Category Fault where
    id = ignore id
    Fault f1 . Fault f2 = Fault $ f1 . pureTry . f2

instance Strong Fault where
    first' = first
    second' = second

instance Choice Fault where
    left' = left
    right' = right

instance Arrow Fault where
    arr = ignore
    (***) x y = dimap fst (,) x <*> lmap snd y

instance ArrowChoice Fault where
    (+++) = choice
    (|||) = decide id

instance ArrowLoop Fault where
    loop = unfirst

---------------------------------------------------------------------
-- Defect
---------------------------------------------------------------------

-- | A value paired with a way to present it.
--
-- @Defect b a@ is an @a@ together with a projection @a -> b@.
-- This is the dual of a lens — instead of focusing /into/ a structure,
-- it carries a value /out/ with its context.
--
-- The concrete @Defect String e@ (an error with its display function)
-- is used in "Control.Exception.Fault.Wrap" as an exception wrapper.
-- The general form is useful anywhere you need a value alongside
-- its rendering, serialization, or classification.
--
-- @
-- Defect displayException myException  :: Defect String SomeException
-- Defect toJSON myValue                :: Defect Value a
-- Defect (const "redacted") secret     :: Defect String Secret
-- @
data Defect b a = Defect (a -> b) a

-- | Extract the projected value.
--
-- @
-- 'defect' ('Defect' show 42) = "42"
-- @
defect :: Defect b a -> b
defect (Defect f a) = f a
{-# INLINE defect #-}

-- | Change the projection.
--
-- @
-- 'redefect' length ('Defect' show 42) = 'Defect' (length . show) 42
-- @
redefect :: (b -> c) -> Defect b a -> Defect c a
redefect g (Defect f a) = Defect (g . f) a
{-# INLINE redefect #-}

---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

-- | Lift a pure function into a 'Fault', re-throwing on exception.
--
-- Avoid using 'ignore' as the final arrow in a composition:
--
-- > ignore f >>> x -- exception-safe
-- > x >>> ignore f -- exceptions thrown by f will propagate
{-# INLINEABLE ignore #-}
ignore :: (a -> b) -> Fault a b
ignore = Fault . either throw

-- | Accept a specific exception type.
--
-- @
-- 'accept' 'displayException' :: 'Exception' e => 'Fault' e 'String'
-- 'accept' 'fromException' :: ('Exception' e1, 'Exception' e2) => 'Fault' e1 ('Maybe' e2)
-- @
{-# INLINEABLE accept #-}
accept :: Exception e => (SomeException -> b) -> Fault e b
accept f = Fault $ either f (f . toException)

-- | Handle a specific exception type.
--
-- @
-- ignore processRequest
--   \<!\> handle \@TimeoutException (\\_ -> defaultResponse)
--   \<!\> handle \@AuthException (\\_ -> forbidden403)
-- @
{-# INLINEABLE handle #-}
handle :: Exception e => (e -> b) -> Fault e b
handle f = Fault $ either (f . unwrap) f
  where unwrap se = case fromException se of
          Just e  -> e
          Nothing -> Ex.throw se

-- | Handle any synchronous exception.
--
-- @
-- ignore riskyComputation
--   \`handleAll\` (\\e -> defaultValue)
-- @
{-# INLINEABLE handleAll #-}
handleAll :: Fault a b -> (SomeException -> b) -> Fault a b
handleAll base handler = Fault $ \case
  Left e  -> handler e
  Right a -> runFault base a

-- | Provide a constant fallback for a specific exception type.
--
-- @
-- ignore parseConfig \<!\> fallback \@ParseException defaultConfig
-- @
{-# INLINEABLE fallback #-}
fallback :: Exception e => b -> Fault e b
fallback def = handle (const def)

---------------------------------------------------------------------
-- Selection
---------------------------------------------------------------------

infixl 3 <!>

-- | Override behavior for a particular exception type.
--
-- @
-- baseHandler \<!\> accept \@ArithException (const fallback)
-- @
{-# INLINEABLE (<!>) #-}
(<!>) :: Exception e => Fault a b -> Fault e b -> Fault a b
(<!>) = flip $ decide f
  where
    f a = unsafePerformIO $ (return $! Right $! a) `Catch.catch` (return . Left)

-- | Contravariant branching.
{-# INLINEABLE decide #-}
decide :: (a -> Either a1 a2) -> Fault a1 b -> Fault a2 b -> Fault a b
decide f x = dimap f (either id id) . choice x

-- | Lift two fault handlers over 'Either'.
{-# INLINEABLE choice #-}
choice :: Fault a1 b1 -> Fault a2 b2 -> Fault (Either a1 a2) (Either b1 b2)
choice x y = Fault $ bimap (unFault x) (unFault y) . coapply

-- | Adapt a fault handler with a function that transforms its evaluation.
{-# INLINEABLE refault #-}
refault :: ((a1 -> b) -> a2 -> b) -> Fault a1 b -> Fault a2 b
refault f = Fault . liftA2 either recover (f . runFault)

---------------------------------------------------------------------
-- Retry
---------------------------------------------------------------------

-- | Retry on any synchronous exception, up to @n@ times.
--
-- @
-- 'runFault' ('retry' 3) riskyComputation
-- @
--
-- Composes with other handlers:
--
-- @
-- 'retry' 3 '>>>' ignore processResult
-- @
{-# INLINEABLE retry #-}
retry :: Int -> Fault a a
retry n = Fault $ either throw go
  where
    go a = case pureTry a of
      Right b -> b
      Left e | n <= 0    -> throw e
             | otherwise -> runFault (retry (n - 1)) a

-- | Retry when a predicate on the exception holds.
--
-- @
-- 'retryWhen' 3 isTransient
-- @
{-# INLINEABLE retryWhen #-}
retryWhen :: Int -> (SomeException -> Bool) -> Fault a a
retryWhen n p = Fault $ either throw go
  where
    go a = case pureTry a of
      Right b -> b
      Left e | n > 0 && p e -> runFault (retryWhen (n - 1) p) a
             | otherwise    -> throw e

---------------------------------------------------------------------
-- Tracing
---------------------------------------------------------------------

-- | Observe exceptions passing through a handler.
--
-- @
-- 'trace' (\\e _ -> log (displayException e)) myHandler
-- @
{-# INLINEABLE trace #-}
trace :: (SomeException -> b -> b) -> Fault a b -> Fault a b
trace f (Fault g) = Fault $ \ea -> case ea of
  Left e  -> f e (g ea)
  Right _ -> g ea

-- | Add context to exceptions via 'CallStacked' from "Control.Exception.Fault.Throw".
-- Re-wraps any exception with the given label prepended to the message.
--
-- @
-- 'annotate' "while processing request" myHandler
-- @
{-# INLINEABLE annotate #-}
annotate :: String -> Fault a b -> Fault a b
annotate msg (Fault g) = Fault $ \case
  Left e  -> g . Left . toException $ Annotated msg e
  Right a -> g (Right a)

-- | Internal: an exception with an annotation. The public version
-- with more features is 'Control.Exception.Fault.Wrap.AnnotatedException'.
data Annotated = Annotated String SomeException
  deriving (Show, Typeable)

instance Exception Annotated where
  displayException (Annotated m e) = m ++ ": " ++ displayException e

---------------------------------------------------------------------
-- Evaluation
---------------------------------------------------------------------

-- | Recover from an exception.
{-# INLINEABLE recover #-}
recover :: Fault a b -> SomeException -> b
recover f = unFault f . Left

-- | Run a fault handler on a value (pure, catches impure exceptions).
{-# INLINEABLE runFault #-}
runFault :: HasCallStack => Fault a b -> a -> b
runFault f = unFault f . pureTry

-- | Run a fault handler on a value, deeply evaluating first.
{-# INLINEABLE runFault' #-}
runFault' :: (HasCallStack, NFData a) => Fault a b -> a -> b
runFault' f = unFault f . pureTryDeep

-- | Run a fault handler on a monadic action (simpler than 'withFault').
--
-- @
-- withFaultIO myHandler (readFile "config.yaml")
-- @
{-# INLINEABLE withFaultIO #-}
withFaultIO :: (HasCallStack, MonadUnliftIO m) => Fault a b -> m a -> m b
withFaultIO f action = unFault f <$> Catch.tryAny (evaluate =<< action)

-- | Run a fault handler in a 'MonadUnliftIO' context with a function argument.
{-# INLINEABLE withFault #-}
withFault :: (HasCallStack, MonadUnliftIO m) => Fault a b -> (r -> m a) -> r -> m b
withFault f g = pure . unFault f <=< Catch.tryAny . (evaluate <=< g)

---------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------

{-# INLINE coapply #-}
coapply :: Either a (Either b1 b2) -> Either (Either a b1) (Either a b2)
coapply = either (Left . Left) (either (Left . Right) (Right . Right))

-- | Re-throw a 'SomeException' (used internally by 'ignore').
throw :: SomeException -> a
throw (SomeException e) = Ex.throw e
