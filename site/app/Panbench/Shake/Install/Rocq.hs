-- | Shake rules for installing @rocq@.
module Panbench.Shake.Install.Rocq
  ( RocqInstallQ(..)
  , defaultRocqOcamlCompiler
  , needRocqInstall
  , rocqInstallRules
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.AllCores
import Panbench.Shake.Git
import Panbench.Shake.Make
import Panbench.Shake.Opam
import Panbench.Shake.Store

import System.FilePath

-- | Shake query for installing @rocq@.
data RocqInstallQ = RocqInstallQ
  { rocqInstallRev :: String
  -- ^ Revision of @rocq@ to use.
  , rocqOcamlCompiler :: String
  -- ^ The @ocaml@ compiler package to use, along with
  -- any associated @ocaml-option-*@ option packages.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Default @ocaml@ compiler to use for @rocq@.
defaultRocqOcamlCompiler :: String
defaultRocqOcamlCompiler = "ocaml-variants.4.14.2+options,ocaml-option-flambda"

-- | Oracle for installing a version of @rocq@.
rocqInstallOracle :: RocqInstallQ -> FilePath -> Action ()
rocqInstallOracle RocqInstallQ{..} storeDir = do
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
      duneCommand_ opamEnv [AddEnv "NJOBS" (show nCores)] ["build", "-p", "rocq-runtime,coq-core,rocq-core,coq"]
    duneCommand_ opamEnv [] ["install", "--prefix=" ++ storeDir, "rocq-runtime", "coq-core", "rocq-core", "coq"]

-- | Require that a particular version of @rocq@ is installed,
-- and return the absolute path pointing to the executable.
needRocqInstall :: RocqInstallQ -> Action FilePath
needRocqInstall q = do
  (store, _) <- askStoreOracle q
  pure (store </> "bin" </> "rocqchk")

-- | Shake rules for installing @rocq@.
rocqInstallRules :: Rules ()
rocqInstallRules = do
  addStoreOracle "_build/store" rocqInstallOracle
  phony "clean-rocq" do
    removeFilesAfter "_build/repos" ["rocq-*"]
