-- | Common classes and types re-exported by the fault library.
--
-- Import this module if you need the class constraints without
-- pulling in any fault-specific functionality.
module Control.Exception.Fault.Class
  ( -- * Exception classes
    Exception (..),
    SomeException (..),
    SomeAsyncException (..),
    Typeable,
    -- * Call stacks
    HasCallStack,
    CallStack,
    callStack,
    -- * IO classes
    MonadIO (..),
    MonadUnliftIO (..),
    -- * Deep evaluation
    NFData,
  )
where

import Control.DeepSeq (NFData)
import Control.Exception (Exception (..), SomeAsyncException (..), SomeException (..))
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO (..))
import Data.Typeable (Typeable)
import GHC.Stack (CallStack, HasCallStack, callStack)
