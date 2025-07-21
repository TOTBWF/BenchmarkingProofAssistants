-- | Shake rules for installing @rocq@.
module Panbench.Shake.Install.Rocq
  ( RocqInstallQ(..)
  , defaultRocqInstallFlags
  , defaultRocqOcamlCompiler
  , needRocqInstall
  , rocqInstallRules
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.AllCores
import Panbench.Shake.File
import Panbench.Shake.Git
import Panbench.Shake.Make
import Panbench.Shake.Opam
import Panbench.Shake.Store

import System.Directory qualified as Dir
import System.FilePath

-- | Shake query for installing @rocq@.
data RocqInstallQ = RocqInstallQ
  { rocqInstallRev :: String
  -- ^ Revision of @rocq@ to use.
  , rocqInstallFlags :: [String]
  , rocqOcamlCompiler :: String
  -- ^ The @ocaml@ compiler package to use, along with
  -- any associated @ocaml-option-*@ option packages.
  , rocqStdlibVersion :: String
  -- ^ Version of the @rocq-stdlib@ package to install from @opam@.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

defaultRocqInstallFlags :: [String]
defaultRocqInstallFlags = []

defaultRocqOcamlCompiler :: String
defaultRocqOcamlCompiler = "ocaml-variants.4.14.1+options,ocaml-option-flambda"

rocqInstallOracle :: Rules ()
rocqInstallOracle =
  addStoreOracle "_build/store" \RocqInstallQ{..} storeDir -> do
    let repoDir = "_build/repos/rocq"
    let workDir = gitRevWorktreePath repoDir rocqInstallRev
    needGitWorktree $ GitWorktreeQ
      { gitWorktreeUpstream = "https://github.com/rocq-prover/rocq.git"
      , gitWorktreeRepo = repoDir
      , gitWorktreeDir = workDir
      , gitWorktreeRev = rocqInstallRev
      }
    withOpamSwitch (LocalSwitch workDir) ["--packages=" ++ rocqOcamlCompiler, "--no-install"] \opamEnv -> do
      needsOpamInstall_ opamEnv ["dune", "ocamlfind", "zarith"]
      command_ (opamEnvOpts opamEnv) "./configure" ["-prefix", storeDir]
      makeCommand_ (opamEnvOpts opamEnv) ["dunestrap"]
      -- We need to use @NJOBS@ over @-j@, see @dev/doc/build-system.dune.md@ for details.
      -- Moreover, note that -p implies --release!
      withAllCores \nCores ->
        duneCommand_ opamEnv [AddEnv "NJOBS" (show nCores)] ["build", "-p", "rocq-runtime,rocq-core"]
      duneCommand_ opamEnv [] ["install", "--prefix=" ++ storeDir, "rocq-runtime", "rocq-core"]

needRocqInstall :: RocqInstallQ -> Action FilePath
needRocqInstall q = do
  (store, _) <- askStoreOracle q
  error "TODO"

rocqInstallRules :: Rules ()
rocqInstallRules = do
  rocqInstallOracle
