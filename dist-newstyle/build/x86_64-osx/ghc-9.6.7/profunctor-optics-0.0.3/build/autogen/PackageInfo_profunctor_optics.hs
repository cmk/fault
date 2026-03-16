{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_profunctor_optics (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "profunctor_optics"
version :: Version
version = Version [0,0,3] []

synopsis :: String
synopsis = "A compact optics library compatible with the typeclasses in profunctors."
copyright :: String
copyright = "2019-2026 Chris McKinlay"
homepage :: String
homepage = "https://github.com/cmk/profunctor-optics"
