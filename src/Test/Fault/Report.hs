{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
--{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
--{-# LANGUAGE TemplateHaskell #-}
--{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE PatternSynonyms #-}
--{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
--{-# LANGUAGE TupleSections #-}
--{-# LANGUAGE TypeApplications #-}
--{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE ViewPatterns #-}

module Test.Fault.Report (
    
    -- * Result
    Result (..),

    -- * Limit
    tests,
    nulls,
    steps,
    Tests,
    Nulls,
    Steps,
    Limit,

    -- * Limits
    limit,
    current,
    atLimit,
    increment,
    Limits (..),
    
    -- * Severity
    Severity (..),

    -- * Record
    notes_,
    outcome_,
    pointers_,
    --severity_,
    result,
    failed,
    collect,
    Record (..),

    -- * Report
    Report,
    Sample (..),
    Failure (..),
    --report,
) where

import Control.DeepSeq (NFData (..))
import Control.Exception (Exception(..), SomeException)
import Data.Function (on)
import Data.Functor.Classes
import Data.Functor.Contravariant
import Data.Functor.Identity
import Data.Functor.Identity
import Data.Map.Strict (Map)
import Data.Map.Strict (Map)
import Data.Maybe (maybeToList)
import Data.Ord (Down(..))
import Data.Semigroup (Max(..), Min (..))
import Data.Text (Text)
import Data.Text (Text)
import Data.Tuple (swap)
import Data.Typeable
import Data.Word
import Data.Word
import GHC.Generics hiding (to)
import GHC.Stack (HasCallStack)
import Prettyprinter (hang, line)
import Control.Arrow ((&&&))
import Data.Profunctor.Optic (Lens', View, lens, over, set, to, view)
import Test.Fault.Render
import qualified Data.Map.Strict as M



-- Result

-------------------------

-- | The result of a test
data Result = Fail | Null | Pass
    deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData Result
deriving via (Min Result) instance Semigroup Result

-- Limit

-------------------------

tests :: Word16 -> Tests
tests = Limit 0

nulls :: Word16 -> Nulls
nulls = Limit 0

steps :: Word16 -> Steps
steps = Limit 0

-- | The number of passed tests for a given size
type Tests = Limit 'Pass

-- | The number of nulled tests for a given size
type Nulls = Limit 'Null

-- | The number of reduction steps for a given size
type Steps = Limit 'Fail

data Limit (r :: Result) = Limit {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
    deriving (Eq, Show, Generic) -- Lift)
    --import Language.Haskell.TH.Syntax (Lift)
    --

instance NFData (Limit r)

instance Semigroup (Limit r) where
    (Limit c1 l1) <> (Limit c2 l2) = Limit (c1 + c2) (max l1 l2)

instance Monoid (Limit r) where
    mempty = Limit 0 0

instance Pretty Tests where
    pretty (Limit c l) = "Tests: " <> pretty c <> " / " <> pretty l

instance Pretty Nulls where
    pretty (Limit c l) = "Nulls: " <> pretty c <> " / " <> pretty l

instance Pretty Steps where
    pretty (Limit c l) = "Steps: " <> pretty c <> " / " <> pretty l

-- Limits

-------------------------

tests_ :: Lens' Limits Tests
tests_ = lens (\(Limits t _ _) -> t) (\(Limits _ n s) t -> Limits t n s)

nulls_ :: Lens' Limits Nulls
nulls_ = lens (\(Limits _ n _) -> n) (\(Limits t _ s) n -> Limits t n s)

steps_ :: Lens' Limits Steps
steps_ = lens (\(Limits _ _ s) -> s) (\(Limits t n _) s -> Limits t n s)

limit_ :: View (Limit r) Word16
limit_ = to (\(Limit _ l) -> l)

current_ :: Lens' (Limit r) Word16
current_ = lens (\(Limit c _) -> c) (\(Limit _ l) c -> Limit c l)

limit :: Result -> Limits -> Word16
limit = \case
    Fail -> view (steps_ . limit_)
    Null -> view (nulls_ . limit_)
    Pass -> view (tests_ . limit_)

current :: Result -> Limits -> Word16
current = \case
    Fail -> view (steps_ . current_)
    Null -> view (nulls_ . current_)
    Pass -> view (tests_ . current_)

atLimit :: Result -> Limits -> Bool
atLimit = \case
    Fail -> view (steps_ . to limited)
    Null -> view (nulls_ . to limited)
    Pass -> view (tests_ . to limited)
  where
    limited (Limit c l) = c >= l

increment :: Result -> Limits -> Limits
increment = \case
    Fail -> over (steps_ . current_) (+ 1)
    Null -> over (nulls_ . current_) (+ 1)
    Pass -> over (tests_ . current_) (+ 1)

data Limits
    = Limits
        -- maximum number of passed tests needed
        {-# UNPACK #-} !Tests
        -- maximum number of nulled tests allowed
        {-# UNPACK #-} !Nulls
        -- maximum number of reduction steps to take for a failed test
        {-# UNPACK #-} !Steps
    deriving (Eq, Show, Generic)

instance NFData Limits

instance Semigroup Limits where
    (Limits p1 n1 s1) <> (Limits p2 n2 s2) = Limits (p1 <> p2) (n1 <> n2) (s1 <> s2)

instance Monoid Limits where
    mempty = Limits mempty mempty mempty

instance Pretty Limits where
    pretty (Limits t0 n0 s0) = "Limits:" <> hang 2 (t <> n <> s)
      where
        t = line <> pretty t0
        n = line <> pretty n0
        s = line <> pretty s0

-- Severity

-------------------------

{- | Severity of a sofware 'Fault'.

  Adapted from the < https://standards.ieee.org/standard/1044-2009.html IEEE Standard Classification for Software Anomalies >,
  Severity is defined by the standard as the highest failure impact that the
  'Fault' could cause.

  Along with its 'Result', the severity of a test determines its ordering with
  respect to other tests, which in turn affects the outcomes of the logical
  operators 'Test.Contra.Test.<&>', 'Test.Contra.Test.<|>', 'Test.Contra.Test.<=>',
  and 'Test.Contra.Test.==>'. In case of ties, the test with higher severity
  is returned.

  Unless otherwise 'Test.Contra.Test.mark'ed, test severity is 'None' by default.
-}
data Severity
    = -- | No significant impact on operations.
      None
    | -- | Non-essential operations are disrupted.
      Minor
    | -- | Essential operations are affected.
      Major
    | -- | Essential operations are disrupted.
      Critical
    deriving (Eq, Bounded, Generic, Show, Ord)

instance NFData Severity
deriving via (Max Severity) instance Semigroup Severity
deriving via (Max Severity) instance Monoid Severity

-- Record

-------------------------

result :: Record -> Result
result = either (const Fail) id . outcome

failed :: Record -> Bool
failed r = result r == Fail

notes_ :: Lens' Record [Ansi]
notes_ = lens (\(Record nts _ _ _) -> nts) (\(Record _ out src sev) n -> Record n out src sev)

outcome_ :: Lens' Record (Either SomeException Result)
outcome_ = lens (\(Record _ out _ _) -> out) (\(Record nts _ src sev) o -> Record nts o src sev)

pointers_ :: Lens' Record [Pointer]
pointers_ = lens (\(Record _ _ src _) -> src) (\(Record nts out _ sev) s -> Record nts out s sev)

{-# INLINE collect #-}
collect :: HasCallStack => [Ansi] -> Either SomeException Result -> Record
collect x y = Record x y (maybeToList $ pointer mempty) None

--type Confidence = Int
data Record = -- | test case labels
    --, classes            :: [String]
    -- ^ test case classes
    --, tables             :: [(String, String)]
    -- ^ test case tables
    Record
    { notes :: ![Ansi] -- TODO: use one Doc with vseps?
    , outcome :: !(Either SomeException Result)
    , pointers :: ![Pointer]
    , -- | Severity of the test case
      severity :: !Severity
    }
    --, labels             :: [String]
    --, labels :: [String]

    deriving (Typeable, Show) --TODO reverse notes

instance Eq Record where
    (==) = on (==) $ either (const Nothing) Just . outcome &&& severity

instance Ord Record where
    compare = on compare $ either (const Nothing) Just . outcome &&& Down . severity

instance Semigroup Record where
    (<>) = min -- logical AND

instance Monoid Record where
    mempty = Record mempty (Right Pass) mempty None


-- Report

-------------------------

{- | A single pretty failure from metadata and source lines.
renderRecord :: T.Text -> Record -> TLB.Builder
renderRecord src rec = renderSource src $ comments rec
, intercalateB "\n\n" $ reverse (notes rec)
-}
type Report g = Either (Failure g) Sample

{- | A software failure.

  Adapted from the < https://standards.ieee.org/standard/1044-2009.html IEEE Standard Classification for Software Anomalies >.

  This data type includes details on where and why a failure occurred.

  See < https://www.iso.org/standard/71952.html ISO/IEC/IEEE 24765:2017 > definition 3.1560.

  A rendered failure looks like this:

 > A failure was detected at
 > → file.hs:1:16
 >   │
 > 1 │   line 1 foo bar do
 >   │ ┌────────────────^^ start label
 > 2 │ │ line 2
 >   │ │      ^ unconnected label
 > 3 │ │ line 3
 > . │ ├──────^ middle label
 > 6 │ │ line 6
 > 7 │ │ line 7 baz end
 >   │ └──────^─────^^^ end label
 >   │        │
 >   │        └ inner label
 >
 > Size: _
 > Seed: _
 > Severity: _
-}
data Failure g = Failure
    { seed :: g
    , size :: Text
    , step :: Steps
    , record :: Record
    }
    deriving (Functor, Show)


-- this is the type you ultimately want to write out
data Sample = -- | test case classes
    -- labels :: !(Map [ByteString] Int)
    -- tables             :: [(String, String)]
    -- ^ test case tables
    -- coverage   :: [(Maybe String, String, Double)]
    -- ^ required coverage
    Sample {limits :: Map Text Limits}
    -- test case labels
    -- classes            :: [String]

    deriving (Show)

instance Pretty Sample where
    --toLogStr r = runLogFmt yamlMap (limits r)
    pretty = viaShow -- TODO render non-failures
    --toLogStr (Sample l (Just f)) = liftA2 (failedf l f) seed record f

instance Semigroup Sample where
    --Sample l1 f1 <> Sample l2 f2 = Sample (M.unionWith (<>) l1 l2) (f1 <|> f2)
    Sample l1 <> Sample l2 = Sample (M.union l1 l2)

instance Monoid Sample where
    mempty = Sample M.empty

