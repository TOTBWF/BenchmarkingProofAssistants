-- | Make-specific rules for @shake@.
module Panbench.Shake.Make
  ( needMake
  , makeCommand_
  , makeCommand
  , makeRules
  ) where

import Data.ByteString qualified as BS

import Development.Shake
import Development.Shake.Classes

import GHC.Generics
import GHC.Stack

import Panbench.Shake.Store

import System.Directory qualified as Dir
import System.Info qualified as Sys

-- | Shake query for finding a GNU @make@ binary.
data MakeQ = MakeQ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

data MakeA = MakeA
  { makeBinPath :: FilePath
  , makeDigest :: BS.ByteString
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult MakeQ = MakeA

-- | Shake query for getting a system-appropriate version of GNU @make@.
needMake :: Action FilePath
needMake = makeBinPath <$> askOracle MakeQ

-- | Oracle for finding GNU @make@.
findMakeOracle :: MakeQ -> Action MakeA
findMakeOracle MakeQ = do
  (liftIO $ Dir.findExecutable makeExecutable) >>= \case
    Just makeBinPath -> do
      makeDigest <- fileDigest makeBinPath
      pure MakeA {..}
    Nothing ->
      fail $ unlines
      [ "Could not find GNU make executable '" ++ makeExecutable ++ "'."
      , "Perhaps it is not installed?"
      ]
  where
    makeExecutable :: String
    makeExecutable
      | Sys.os `elem` ["darwin", "freebsd", "netbsd", "openbsd"] = "gmake"
      | otherwise = "make"

-- | Run @'makeExecutable'@, and ignore the result.
--
-- See @'command_'@ for more documentation.
makeCommand_ :: (HasCallStack) => [CmdOption] -> [String] -> Action ()
makeCommand_ opts args = do
  make <- needMake
  command_ opts make args

-- | Run @'makeExecutable'@, and capture the result.
--
-- See @'command'@ for more documentation.
makeCommand :: (HasCallStack, CmdResult r) => [CmdOption] -> [String] -> Action r
makeCommand opts args = do
  make <- needMake
  command opts make args

-- | Shake rules for GNU @mak@.
makeRules :: Rules ()
makeRules = do
  _ <- addOracle findMakeOracle
  pure ()
