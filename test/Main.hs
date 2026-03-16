{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Category (id, (.))
import Control.Exception (ArithException(..), ArrayException(..),
  Exception(..), SomeException, toException)
import Control.Exception.Fault.Type
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Prelude hiding ((.), id)

ident :: a -> a
ident x = x

comp :: (b -> c) -> (a -> b) -> a -> c
comp f g x = f (g x)

---------------------------------------------------------------------
-- Exception generators
---------------------------------------------------------------------

genArith :: Gen ArithException
genArith = Gen.element [Overflow, Underflow, LossOfPrecision, DivideByZero, Denormal, RatioZeroDenominator]

genArray :: Gen ArrayException
genArray = Gen.choice
  [ IndexOutOfBounds <$> Gen.string (Range.linear 0 20) Gen.alpha
  , UndefinedElement <$> Gen.string (Range.linear 0 20) Gen.alpha
  ]

-- | Generate a SomeException that's either an ArithException or ArrayException.
genSomeException :: Gen SomeException
genSomeException = Gen.choice
  [ toException <$> genArith
  , toException <$> genArray
  ]

genInt :: Gen Int
genInt = Gen.int (Range.linearFrom 0 (-1000) 1000)

genFun :: Gen (Int -> Int)
genFun = Gen.element [(+1), (*2), negate, abs, \x -> x * x]

---------------------------------------------------------------------
-- Category laws
---------------------------------------------------------------------

prop_category_left_id :: Property
prop_category_left_id = property $ do
  a <- forAll genInt
  runFault (id . ignore (+1)) a === runFault (ignore (+1)) a

prop_category_right_id :: Property
prop_category_right_id = property $ do
  a <- forAll genInt
  runFault (ignore (+1) . id) a === runFault (ignore (+1)) a

prop_category_assoc :: Property
prop_category_assoc = property $ do
  a <- forAll genInt
  let f = ignore (+1)
      g = ignore (*2)
      h = ignore negate
  runFault ((f . g) . h) a === runFault (f . (g . h)) a

---------------------------------------------------------------------
-- ignore / runFault round-trip
---------------------------------------------------------------------

prop_ignore_roundtrip :: Property
prop_ignore_roundtrip = property $ do
  a <- forAll genInt
  runFault (ignore (+1)) a === a + 1

prop_ignore_id :: Property
prop_ignore_id = property $ do
  a <- forAll genInt
  runFault (ignore ident) a === a

---------------------------------------------------------------------
-- (<!>) recovery
---------------------------------------------------------------------

-- | A handler for ArithException always catches ArithException.
prop_override_recovery :: Property
prop_override_recovery = property $ do
  e <- forAll genArith
  let f = ignore @Int @Int ident
      k = const 42
      h = f <!> handle @ArithException k
  recover h (toException e) === 42

-- | A handler for ArithException does not catch ArrayException.
-- We use `handleAll` to observe the pass-through rather than letting it throw.
prop_override_passthrough :: Property
prop_override_passthrough = property $ do
  e <- forAll genArray
  let base = (ignore @Int @Int ident) `handleAll` (const (-1))
      h = base <!> handle @ArithException (const 999)
  -- ArrayException should NOT go to the ArithException handler (999)
  -- It should fall through to base's handleAll (which returns -1)
  recover h (toException e) === (-1)

-- | Chaining: last override for a type wins.
prop_override_last_wins :: Property
prop_override_last_wins = property $ do
  e <- forAll genArith
  let f = ignore @Int @Int ident
      h1 = f <!> handle @ArithException (const 1)
      h2 = h1 <!> handle @ArithException (const 2)
  recover h2 (toException e) === 2

-- | Non-overlapping handlers are independent.
prop_override_nonoverlapping :: Property
prop_override_nonoverlapping = property $ do
  arithE <- forAll genArith
  arrayE <- forAll genArray
  let f = ignore @Int @Int ident
      h = f <!> handle @ArithException (const 1)
              <!> handle @ArrayException (const 2)
  recover h (toException arithE) === 1
  recover h (toException arrayE) === 2

---------------------------------------------------------------------
-- handleDefect
---------------------------------------------------------------------

prop_handleDefect_id :: Property
prop_handleDefect_id = property $ do
  a <- forAll genInt
  let d = Defect a (+1)
  defect (handleDefect (ignore ident) d) === defect d

prop_handleDefect_compose :: Property
prop_handleDefect_compose = property $ do
  a <- forAll genInt
  let d = Defect a ident
      f = ignore (+1)
      g = ignore (*2)
  defect (handleDefect f (handleDefect g d)) === defect (handleDefect (f . g) d)

---------------------------------------------------------------------
-- Defect Functor
---------------------------------------------------------------------

prop_defect_functor_id :: Property
prop_defect_functor_id = property $ do
  a <- forAll genInt
  defect (fmap ident (Defect a (+1))) === defect (Defect a (+1))

prop_defect_functor_compose :: Property
prop_defect_functor_compose = property $ do
  a <- forAll genInt
  let d = Defect a ident
      f = (+1)
      g = (*2)
  defect (fmap (f `comp` g) d) === defect (fmap f (fmap g d))

---------------------------------------------------------------------
-- Runner
---------------------------------------------------------------------

main :: IO ()
main = do
  result <- checkParallel $ Group "Control.Exception.Fault"
    [ ("category_left_id", prop_category_left_id)
    , ("category_right_id", prop_category_right_id)
    , ("category_assoc", prop_category_assoc)
    , ("ignore_roundtrip", prop_ignore_roundtrip)
    , ("ignore_id", prop_ignore_id)
    , ("override_recovery", prop_override_recovery)
    , ("override_passthrough", prop_override_passthrough)
    , ("override_last_wins", prop_override_last_wins)
    , ("override_nonoverlapping", prop_override_nonoverlapping)
    , ("handleDefect_id", prop_handleDefect_id)
    , ("handleDefect_compose", prop_handleDefect_compose)
    , ("defect_functor_id", prop_defect_functor_id)
    , ("defect_functor_compose", prop_defect_functor_compose)
    ]
  if result then pure () else error "tests failed"
