{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_coapplicative (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "coapplicative"
version :: Version
version = Version [0,0,2] []

synopsis :: String
synopsis = "Coapplicative functors"
copyright :: String
copyright = "2020 Chris McKinlay"
homepage :: String
homepage = "https://github.com/cmk/coapplicative"
