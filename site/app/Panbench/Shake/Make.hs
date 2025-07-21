-- | Make-specific rules for @shake@.
module Panbench.Shake.Make
  ( makeExecutable
  , makeCommand_
  , makeCommand
  ) where

import Development.Shake

import GHC.Stack

import System.Info qualified as Sys

-- | The name of the appropriate @make@ executable to use.
--
-- On Windows and Linux this is just @make@, but on MacOS and BSD variants
-- we prefer to use @gmake@.
makeExecutable :: String
makeExecutable
  | Sys.os `elem` ["darwin", "freebsd", "netbsd", "openbsd"] = "gmake"
  | otherwise = "make"

-- | Run @'makeExecutable'@, and ignore the result.
--
-- See @'command_'@ for more documentation.
makeCommand_ :: (HasCallStack) => [CmdOption] -> [String] -> Action ()
makeCommand_ opts args = command_ opts makeExecutable args

-- | Run @'makeExecutable'@, and capture the result.
--
-- See @'command'@ for more documentation.
makeCommand :: (HasCallStack, CmdResult r) => [CmdOption] -> [String] -> Action r
makeCommand opts args = command opts makeExecutable args
