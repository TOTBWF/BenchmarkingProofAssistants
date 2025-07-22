-- | Helpers for installing @lean@.
module Panbench.Shake.Install.Lean
  ( LeanInstallQ(..)
  , defaultLeanCMakeFlags
  , defaultLeanMakeFlags
  , needLeanInstall
  , leanInstallRules
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.AllCores
import Panbench.Shake.File
import Panbench.Shake.Git
import Panbench.Shake.Store

import System.Directory qualified as Dir
import System.FilePath

-- | Query for installing a version of @lean@.
data LeanInstallQ = LeanInstallQ
  { leanInstallRev :: String
  -- ^ Revision of Lean to install.
  , leanCMakeFlags :: [String]
  -- ^ CMake flags used to build Lean.
  , leanMakeFlags :: [String]
  -- ^ Make flags used to build Lean.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult LeanInstallQ = FilePath

-- | Default flags to pass to @cmake@ when compiling lean.
defaultLeanCMakeFlags :: [String]
defaultLeanCMakeFlags = ["--preset", "release"]

-- | Default flags to pass to @make@ when compiling lean.
defaultLeanMakeFlags :: [String]
defaultLeanMakeFlags = []

-- | Oracle for installing a version of Lean 4.
--
-- The oracle returns the absolute path to the produced @lean@ binary.
leanInstall :: LeanInstallQ -> FilePath -> Action ()
leanInstall LeanInstallQ{..} storeDir = do
  let repoDir = "_build/repos/lean"
  let workDir = replaceDirectory storeDir "_build/repos"
  needGitWorktree $ GitWorktreeQ
    { gitWorktreeUpstream = "https://github.com/leanprover/lean4.git"
    , gitWorktreeRepo = repoDir
    , gitWorktreeDir = workDir
    , gitWorktreeRev = leanInstallRev
    }
  withAllCores \nCores -> do
    command_ [Cwd workDir] "cmake" leanCMakeFlags
    command_ [Cwd workDir] "make" (["stage3", "-C", "build/release", "-j" ++ show nCores] ++ leanMakeFlags)
  copyDirectoryRecursive (workDir </> "build" </> "release" </> "stage3") storeDir

-- | Require that a particular version of @lean@ is installed,
-- and return the absolute path pointing to the executable.
needLeanInstall :: LeanInstallQ -> Action FilePath
needLeanInstall q = do
  (store, _) <- askStoreOracle q
  liftIO $ Dir.makeAbsolute (store </> "bin" </> "lean")

-- | Shake rules for installing @lean@.
leanInstallRules :: Rules ()
leanInstallRules = do
  addStoreOracle "lean" leanInstall
  phony "clean-lean" do
    removeFilesAfter "_build/repos" ["lean-*"]
    removeFilesAfter "_build/store" ["lean-*"]
