{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_fault (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "fault"
version :: Version
version = Version [0,0,1] []

synopsis :: String
synopsis = "A profunctor for fault-tolerant computation."
copyright :: String
copyright = "2019-2026 Chris McKinlay"
homepage :: String
homepage = "https://github.com/cmk/fault"
