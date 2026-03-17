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

    -- * AnnotatedException
    AnnotatedException (..),

    -- * Fault + Defect
    handleDefect,

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

    -- * Pure evaluation
    recover,
    runFault,
    runFault',
    pureTry,
    pureTryDeep,

) where

import Control.Applicative
import Control.Arrow (Arrow(..), ArrowChoice(..), ArrowLoop(..))
import Control.Category (Category)
import qualified Control.Category as C
import Control.DeepSeq (NFData(rnf))
import Control.Exception.Fault.Class
import qualified Control.Exception as Ex
import Control.Monad
import Data.Bifunctor (bimap)
import Data.Distributive (Distributive(..))
import Data.Profunctor (Profunctor(..), Strong(..), Choice(..), Closed(..), Costrong(..), Cochoice(..))
import Data.Profunctor.Rep (Corepresentable(..))
import Data.Profunctor.Sieve (Cosieve(..))
import Data.Profunctor.Types (Costar(..))
import Prelude
import System.IO.Unsafe (unsafePerformIO)

-- | Try to evaluate a value purely, catching any impure exceptions.
pureTry :: a -> Either SomeException a
pureTry a = unsafePerformIO $
  (return $! Right $! a) `Ex.catch` \e -> return (Left (e :: SomeException))
{-# INLINE pureTry #-}

-- | Like 'pureTry', but fully evaluates via 'NFData' first.
pureTryDeep :: NFData a => a -> Either SomeException a
pureTryDeep a = unsafePerformIO $
  (Right <$> Ex.evaluate (rnf a `seq` a)) `Ex.catch` \e -> return (Left (e :: SomeException))
{-# INLINE pureTryDeep #-}

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

deriving via (Costar (Either SomeException)) instance Profunctor Fault
deriving via (Costar (Either SomeException)) instance Closed Fault
deriving via (Costar (Either SomeException)) instance Costrong Fault
deriving via (Costar (Either SomeException)) instance Cochoice Fault
instance Cosieve Fault (Either SomeException) where
  cosieve (Fault f) = f

instance Corepresentable Fault where
  type Corep Fault = Either SomeException
  cotabulate f = Fault f
deriving via (Costar (Either SomeException) a) instance Functor (Fault a)
deriving via (Costar (Either SomeException) a) instance Applicative (Fault a)
deriving via (Costar (Either SomeException) a) instance Monad (Fault a)
instance Distributive (Fault a) where
  distribute fs = Fault $ \ea -> fmap (\(Fault f) -> f ea) fs
deriving via ((->) (Either SomeException a) b) instance Semigroup b => Semigroup (Fault a b)
deriving via ((->) (Either SomeException a) b) instance Monoid b => Monoid (Fault a b)

-- | 'Category' instance composes fault handlers left-to-right.
--
-- Properties:
--
-- [Left identity]  @'id' '>>>' f ≡ f@
-- [Right identity] @f '>>>' 'id' ≡ f@
-- [Associativity]  @(f '>>>' g) '>>>' h ≡ f '>>>' (g '>>>' h)@
instance Category Fault where
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
-- If a failure type was created by an external library it may not
-- provide an 'Exception' instance, which means you can't throw it.
-- 'Defect' provides a solution for this.
--
-- @Defect a b@ is an @a@ together with a projection @a -> b@.
-- @Defect a@ is a 'Functor' in @b@ — you can post-compose the
-- projection with 'fmap'.
--
-- 'Defect' and 'Fault' are duals: a @Defect a b@ holds a value
-- that /could become/ a @b@ (via its projection), while a
-- @Fault b c@ /handles/ values that may have already failed.
-- They compose via 'handleDefect':
--
-- @
-- 'handleDefect' myHandler myDefect :: Defect a c
-- @
--
-- A @Defect a SomeException@ is a value that could become an
-- exception; a @Fault b c@ is something that handles exceptions.
-- Together they form a complete error pipeline: construct a defect,
-- project it, handle the result.
--
-- The concrete @Defect e String@ (an error with its display function)
-- is used in "Control.Exception.Fault.Wrap" as a throwable exception
-- wrapper, avoiding the need for a 'Show' instance on the error type.
--
-- @
-- Defect myException displayException :: Defect SomeException String
-- Defect myValue toJSON               :: Defect a Value
-- Defect secret (const "redacted")    :: Defect Secret String
-- @
data Defect a b = Defect a (a -> b)

-- | Extract the projected value.
--
-- @
-- 'defect' ('Defect' 42 show) = "42"
-- @
defect :: Defect a b -> b
defect (Defect a f) = f a
{-# INLINE defect #-}

instance Functor (Defect a) where
  fmap g (Defect a f) = Defect a (g . f)

-- | @'Defect' e String@ is throwable: the projection provides
-- 'displayException', avoiding the need for a 'Show' instance on @e@.
instance Typeable e => Show (Defect e String) where
  showsPrec p d =
    showParen (p > 10) $
      showString "Defect _ " . showsPrec 11 (defect d)

instance Typeable e => Exception (Defect e String) where
  displayException = defect

---------------------------------------------------------------------
-- AnnotatedException
---------------------------------------------------------------------

-- | An exception annotated with a context message.
--
-- Used by 'annotate' to add context as exceptions propagate
-- through composed handlers. Builds up a chain of context:
--
-- @
-- "while processing request: while parsing body: parse error at line 5"
-- @
--
-- Unlike 'Control.Exception.Fault.Wrap.DisplayedException', which only
-- fixes how GHC /renders/ an exception, 'AnnotatedException' enriches
-- the exception with additional information. The original exception is
-- preserved inside and can still be extracted with 'fromException'.
data AnnotatedException = AnnotatedException String SomeException
  deriving (Show, Typeable)

instance Exception AnnotatedException where
  displayException (AnnotatedException msg e) =
    msg ++ ": " ++ displayException e

---------------------------------------------------------------------
-- Fault + Defect
---------------------------------------------------------------------

-- | Apply a fault handler to the projection of a defect.
--
-- @
-- 'handleDefect' ('retry' 3) ('Defect' riskyValue id) :: Defect a a
-- @
--
-- This composes the handler with the defect's projection,
-- keeping the original value intact.
--
-- Properties:
--
-- [Identity]    @'handleDefect' ('ignore' id) d ≡ d@
-- [Composition] @'handleDefect' f ('handleDefect' g d) ≡ 'handleDefect' (f '.' g) d@
handleDefect :: Fault b c -> Defect a b -> Defect a c
handleDefect f (Defect a g) = Defect a (runFault f . g)
{-# INLINE handleDefect #-}

---------------------------------------------------------------------
-- Construction
---------------------------------------------------------------------

-- | Lift a pure function into a 'Fault', re-throwing on exception.
--
-- Properties:
--
-- [Round-trip] @'runFault' ('ignore' f) a ≡ f a@ (when @f a@ does not throw)
-- [Identity]   @'ignore' 'id' ≡ 'Control.Category.id'@
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
-- | Override behavior for a particular exception type.
--
-- On the 'Left' (exception) path, if the exception matches type @e@
-- via 'fromException', route to the override handler; otherwise use
-- the base. The 'Right' (value) path always uses the base.
--
-- Properties:
--
-- [Recovery] @'recover' (f \<!\> 'handle' \@E k) ('toException' e) ≡ k e@
--
-- [Pass-through] If @'fromException' \@E se ≡ Nothing@, then
-- @'recover' (f \<!\> 'handle' \@E k) se ≡ 'recover' f se@
--
-- [Right-biased] The rightmost handler for a given type wins:
-- @f \<!\> g \<!\> h@ tries @h@ first, then @g@, then @f@.
{-# INLINEABLE (<!>) #-}
(<!>) :: forall e a b. Exception e => Fault a b -> Fault e b -> Fault a b
(<!>) (Fault base) (Fault handler) = Fault $ \case
  Left se -> case Ex.fromException se of
    Just e  -> handler (Right e)
    Nothing -> base (Left se)
  Right a -> base (Right a)

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
  Left e  -> g . Left . toException $ AnnotatedException msg e
  Right a -> g (Right a)

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
runFault' :: HasCallStack => NFData a => Fault a b -> a -> b
runFault' f = unFault f . pureTryDeep

---------------------------------------------------------------------
-- Internal
---------------------------------------------------------------------

{-# INLINE coapply #-}
coapply :: Either a (Either b1 b2) -> Either (Either a b1) (Either a b2)
coapply = either (Left . Left) (either (Left . Right) (Right . Right))

-- | Re-throw a 'SomeException' (used internally by 'ignore').
throw :: SomeException -> a
throw (SomeException e) = Ex.throw e
