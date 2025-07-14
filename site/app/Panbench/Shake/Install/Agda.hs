-- | Helpers for installing @agda@.
module Panbench.Shake.Install.Agda
  ( AgdaInstallQ(..)
  , needAgdaInstall
  , agdaInstallRules
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.Git

-- | Query for installing a version of agda
data AgdaInstallQ = AgdaInstallQ
  { agdaInstallRev :: String
  -- ^ Revision of Agda to install.
  , agdaInstallFlags :: [String]
  -- ^ Compile flags used to build Agda.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult AgdaInstallQ = FilePath

agdaInstallOracle :: Rules (AgdaInstallQ -> Action FilePath)
agdaInstallOracle =
  addOracle \AgdaInstallQ{..} -> do
    let repoDir = "_build/repos/agda"
    let workDir = repoDir ++ "-" ++ agdaInstallRev
    needGitWorktree $ GitWorktreeQ
      { gitWorktreeUpstream = "https://github.com/agda/agda.git"
      , gitWorktreeRepo = repoDir
      , gitWorktreeDir = workDir
      , gitWorktreeRev = agdaInstallRev
      }
    -- [TODO: Reed M, 14/07/2025] We could be more reproducible by allowing the
    -- user to specify a cabal lockfile.
    --
    -- Note that this also uses the system GHC: we could make this more configurable by
    -- calling out to @ghcup@, but let's just get things working for now
    Stdout bin <- command [Cwd workDir] "cabal" ["list-bin", "agda", "--project-dir=."]
    -- [FIXME: Reed M, 14/07/2025] This won't invalidate the build if the flags change.
    doesFileExist bin >>= \case
      True -> pure ()
      False -> command [Cwd workDir] "cabal" (["build", "agda", "--project-dir=."] ++ agdaInstallFlags)
    pure bin

-- | Require that a particular version of @agda@ is installed,
-- and return the absolute path pointing to the executable.
needAgdaInstall :: AgdaInstallQ -> Action FilePath
needAgdaInstall = askOracle

-- | Shake rules for installing @agda@.
agdaInstallRules :: Rules ()
agdaInstallRules = do
  _ <- agdaInstallOracle
  pure ()
