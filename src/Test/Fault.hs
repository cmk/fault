{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{- | Errors, faults, and exceptions.

 __Note__: The contents of this module are subject to change, including during
 minor version updates. Don't import it directly unless you need to.
-}
module Test.Fault (
    
    -- * Fault
    Fault (..),
    ignore,
    accept,
    
    -- ** Selection
    (<!>), 
    decide,
    refault,
    
    -- ** Evaluation
    recover,
    runFault,
    runFault',
    withFault,
    
    -- * Testable
    protect,
    overlay,
    Testable (..),

    -- * Exceptions
    throw,
    display,
    exception,
    UE.toSyncException,
    UE.isSyncException,
    UE.isAsyncException,
    UE.stringException,
    UE.mapExceptionM,
    UE.fromExceptionUnwrap,
    SomeException (..),

    -- ** Recovery
    UE.pureTry,
    UE.pureTryDeep,
    UE.evaluate,
    UE.try,
    UE.tryIO,
    UE.tryAny,
    UE.tryDeep,
    UE.trySyncOrAsync,
    UE.tryJust,

    -- ** Cleanup
    UE.bracket,
    UE.bracketOnError,
    UE.finally,
    UE.onException,

    -- ** Class
    HasCallStack,
    withFrozenCallStack,
    Exception (..),
    MonadIO (..),
    MonadUnliftIO (..),
) where

-- Category())

import Control.Applicative
import Control.Category (Category, (<<<), (>>>))
import qualified Control.Category as C
import Control.DeepSeq -- (NFData (..), ($!!))
import Control.Exception as E
import Control.Monad
import Data.Bifunctor (bimap)
import Data.Bool
import Data.Char (isSpace)
import Data.Functor.Classes
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Maybe (maybeToList, isJust, mapMaybe)
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Typeable
import GHC.Generics
import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), callStack, getCallStack, withFrozenCallStack)
import System.IO
import Prelude

import System.IO.Unsafe (unsafePerformIO)
import Text.Show
import UnliftIO (MonadIO (..), MonadUnliftIO (..))
import qualified UnliftIO.Exception as UE -- Exception (onException, bracket, bracket_, bracketOnError_, finally, withException)

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import Control.Arrow (Arrow(..), ArrowChoice(..), ArrowLoop(..))
import Data.Profunctor.Optic (Profunctor(..), Strong(..), Choice(..), Closed(..), Costrong(..), Cochoice(..), Costar(..), over, set)
import Test.Fault.Render
import Test.Fault.Report

-- Fault

-------------------------

{- | A software fault.

 Adapted from the < https://standards.ieee.org/standard/1044-2009.html IEEE Standard Classification for Software Anomalies >.

 A 'Fault' is a condition that causes a software routine or program to fail to
 perform its required function.

 See < https://www.iso.org/standard/71952.html ISO/IEC/IEEE 24765:2017 > definition 3.15693.
-}
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

    Fault f1 . Fault f2 = Fault $ f1 . UE.pureTry . f2

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

{- | 

 Ignore a possible exception. Avoid using 'ignore' as the final arrow in a composition:

  > ignore f >>> x -- exception-safe
  > x >>> ignore f -- exceptions thrown by f will propagate
-}
{-# INLINEABLE ignore #-}
ignore :: (a -> b) -> Fault a b
ignore = Fault . either throw

{- | Accept a specific exception.

 @
 'accept' 'displayException' :: 'Exception' e => 'Fault' e 'String'
 'accept' 'isSyncException' :: 'Exception' e => 'Fault' e 'Bool'
 'accept' 'fromException' :: ('Exception' e1, 'Exception' e2) => 'Fault' e1 ('Maybe' e2)
 @

-}
{-# INLINEABLE accept #-}
accept :: Exception e => (SomeException -> b) -> Fault e b
accept f = Fault $ either f (f . toException)

-- Evaluation

-------------------------

{-# INLINEABLE recover #-}
recover :: Fault a b -> SomeException -> b
recover f = unFault f . Left

{-# INLINEABLE runFault #-}
runFault :: HasCallStack => Fault a b -> a -> b
runFault f = unFault f . UE.pureTry

{-# INLINEABLE runFault' #-}
runFault' :: (HasCallStack, NFData a) => Fault a b -> a -> b
runFault' f = unFault f . UE.pureTryDeep

{- |
 @'withFault' 'test' :: ('MonadUnliftIO' m, 'Testable' t) => (a -> m t) -> a -> m 'Record'@
-}
{-# INLINEABLE withFault #-}
withFault :: (HasCallStack, MonadUnliftIO m) => Fault a b -> (r -> m a) -> r -> m b
withFault f g = pure . unFault f <=< UE.tryAny . (UE.evaluate <=< g)
-- NB: The order of function composition is important, as we want to
-- catch both throw and throwIO.

-- Selection

-------------------------

infixl 3 <!>
{- | Override behavior for a particular exception.

 > except $ accept @E.ArithException (exception Major)

  >>> :set -XOverloadedStrings
  >>> import qualified Control.Exception as E
  >>> e = except @ArithException (fmt1 $ const "Oops") $
  >>> checkIO Nothing (E.assert False False) $ predicate e id
  (Fail,"--- Exception (SomeException) ---\nAssertion failed\nCallStack (from HasCallStack):\n  assert, called at <interactive>:62:18 in interactive:Ghci2\n")
  >>> checkIO Nothing (E.throw Overflow) $ predicate e id
  (Fail,"Oops")
-}
{-# INLINEABLE (<!>) #-}
(<!>) :: Exception e => Fault a b -> Fault e b -> Fault a b
(<!>) = flip $ decide f
  where
    f a = unsafePerformIO $ (return $! Right $! a) `UE.catch` (return . Left)

{- | A contravariant analogue of '<|>'

 See the < http://hackage.haskell.org/package/contravariant/docs/Data-Functor-Contravariant-Divisible.html#t:Decidable contravariant > package.
-}
{-# INLINEABLE decide #-}
decide :: (a -> Either a1 a2) -> Fault a1 b -> Fault a2 b -> Fault a b
decide f x = dimap f (either id id) . choice x

{-# INLINEABLE choice #-}
choice :: Fault a1 b1 -> Fault a2 b2 -> Fault (Either a1 a2) (Either b1 b2)
choice x y = Fault $ bimap (unFault x) (unFault y) . coapply

{-# INLINE coapply #-}
coapply :: Either a (Either b1 b2) -> Either (Either a b1) (Either a b2)
coapply = either (Left . Left) (either (Left . Right) (Right . Right))

{-# INLINEABLE refault #-}
refault :: ((a1 -> b) -> a2 -> b) -> Fault a1 b -> Fault a2 b
refault f = Fault . liftA2 either recover (f . runFault)

-- Testable

-------------------------

class Typeable t => Testable t where
    test :: HasCallStack => Fault t Record

instance Testable () where
    test = protect id $ Fault $ collect <$> showE <*> (Pass <$)

instance Testable Bool where
    test = protect id $ Fault $ collect <$> showE <*> (bool Fail Pass <$>)

instance Testable Record where
    test = protect id $ Fault $ flip either id $ recover $ test @SomeException

instance Testable Result where
    test = protect id $ Fault $ collect <$> showE <*> id

instance Testable SomeException where
    test = Fault $ collect <$> showE <*> (Fail <$)

instance Testable t => Testable (Maybe t) where
    test = protect id $ decide (maybe (Left nil) Right) (arr id) test
      where
        nil = collect mempty (pure Null)

instance Testable t => Testable [t] where
    test = over notes_ (pure . render) <$> go
      where
        -- Render a list of result strings to a list of results string.
        render l = "[" <> foldMap id (L.intersperse "," l) <> "]"

        go = protect id $ decide match cons (arr id)

        match (h : []) = Right $ runFault test h
        match (h : t) = h `seq` Left (h, t)
        match [] = Right $ collect mempty (pure Null)

        cons = fmap meet $ test *** go
        meet (r1, r2) = set notes_ (notes r1 <> notes r2) (min r1 r2)

instance Testable t => Testable (NonEmpty t) where
    test = protect id $ dimap (N.head &&& N.tail) (uncurry (<>)) $ test *** test

instance (Testable t1, Testable t2) => Testable (t1, t2) where
    test = protect id $ fmap (uncurry (<>)) $ test *** test

instance (Testable t1, Testable t2) => Testable (Either t1 t2) where
    test = protect id $ decide id test test

{-# INLINE showE #-}
showE :: Show a => Either SomeException a -> [Ansi]
showE = either (pure . pretty . displayException) (pure . viaShow)

{-# INLINEABLE protect #-}
protect :: Exception e => (e -> SomeException) -> Fault r Record -> Fault r Record
protect e f = f <!> lmap e test

{-# INLINEABLE overlay #-}
overlay :: (HasCallStack, Testable t) => T.Text -> Fault t Record
overlay t = over pointers_ (<> maybeToList (pointer t)) <$> test

-- Exceptions

-------------------------

exception :: Exception e => e -> Record
exception = recover (test @SomeException) . toException

display :: (HasCallStack, Exception e) => e -> Doc ann
display ex = withFrozenCallStack msg
  where
    msg =
        pretty $
            show (typeOf ex) ++ ": " ++ L.dropWhileEnd isSpace (displayException ex)
