-- | Shake rules for @rocq@.
module Panbench.Shake.Lang.Rocq
  ( -- $shakeRocqInstall
    RocqQ(..)
  , defaultRocqInstallRev
  , defaultRocqOcamlCompiler
  , needRocqInstallOpts
  , needRocq
  -- $shakeRocqCommands
  , rocqCheckDefaultArgs
  , rocqDoctor
  -- $shakeRocqRules
  , rocqRules
  ) where

import Data.List

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.AllCores
import Panbench.Shake.Git
import Panbench.Shake.Make
import Panbench.Shake.Opam
import Panbench.Shake.Store

import System.FilePath

-- * Rocq Installation
--
-- $shakeRocqInstall

-- | Shake query for installing @rocq@.
data RocqQ = RocqQ
  { rocqInstallRev :: String
  -- ^ Revision of @rocq@ to use.
  , rocqOcamlCompiler :: String
  -- ^ The @ocaml@ compiler package to use, along with
  -- any associated @ocaml-option-*@ option packages.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Default @rocq@ revision.
defaultRocqInstallRev :: String
defaultRocqInstallRev = "V9.0.0"

-- | Default @ocaml@ compiler to use for @rocq@.
defaultRocqOcamlCompiler :: String
defaultRocqOcamlCompiler = "ocaml-variants.4.14.2+options,ocaml-option-flambda"

-- | Docs for the @install-rocq@ rule.
rocqInstallDocs :: String
rocqInstallDocs = unlines
  [ "Install a version of rocq."
  , ""
  , "Can be configured with the following environment variables:"
  , "* $ROCQ_VERSION: select the revision of rocq to install "
  , "  Defaults to " <> defaultRocqInstallRev
  , "* $ROCQ_OCAML: select the version of ocaml to use to build rocq."
  , "  Defaults to " <> defaultRocqOcamlCompiler
  ]

-- | Get the @rocq@ version to install from the @$ROCQ_VERSION@ environment variable.
needRocqInstallRev :: Action String
needRocqInstallRev = getEnvWithDefault defaultRocqInstallRev "ROCQ_VERSION"

-- | Get the @ocaml@ compiler used to install @rocq@ from the @$ROCQ_OCAML@ environment variable.
needRocqInstallOcaml :: Action String
needRocqInstallOcaml = getEnvWithDefault defaultRocqOcamlCompiler "ROCQ_OCAML"

-- | Get install options for @rocq@ from environment variables.
needRocqInstallOpts :: Action RocqQ
needRocqInstallOpts = do
  rocqInstallRev <- needRocqInstallRev
  rocqOcamlCompiler <- needRocqInstallOcaml
  pure RocqQ {..}

-- | Run a command with access to a Rocq git worktree.
withRocqWorktree
  :: String -- ^ Revision of Rocq to check out.
  -> FilePath -- ^ Store directory.
  -> (FilePath -> Action a) -- ^ Action, parameterized by the worktree directory.
  -> Action a
withRocqWorktree rev storeDir act =
  let repoDir = "_build/repos/rocq"
      workDir = replaceDirectory storeDir "_build/repos"
      worktree = GitWorktreeQ
        { gitWorktreeUpstream = "https://github.com/rocq-prover/rocq.git"
        , gitWorktreeRepo = repoDir
        , gitWorktreeDir = workDir
        , gitWorktreeRev = rev
        }
  in withGitWorktree worktree (act workDir)

-- | Oracle for installing a version of @rocq@.
rocqInstallOracle :: RocqQ -> FilePath -> Action ()
rocqInstallOracle RocqQ{..} storeDir = do
  withRocqWorktree rocqInstallRev storeDir \workDir -> do
    let rocqSwitchPkgs = intercalate "," [rocqOcamlCompiler, "dune", "ocamlfind", "zarith"]
    withOpamSwitch (LocalSwitch workDir) ["--packages=" ++ rocqSwitchPkgs, "--no-install"] \opamEnv -> do
      command_ (opamEnvOpts opamEnv) "./configure" ["-prefix", storeDir]
      makeCommand_ (opamEnvOpts opamEnv) ["dunestrap"]
      -- We need to use @NJOBS@ over @-j@, see @dev/doc/build-system.dune.md@ for details.
      -- Moreover, note that -p implies --release!
      withAllCores \nCores ->
        duneCommand_ opamEnv [AddEnv "NJOBS" (show nCores)] ["build", "-p", "rocq-runtime,coq-core,rocq-core,coq"]
      duneCommand_ opamEnv [] ["install", "--prefix=" ++ storeDir, "rocq-runtime", "coq-core", "rocq-core", "coq"]

-- | Require that a particular version of @rocq@ is installed,
-- and return the absolute path pointing to the executable.
needRocq :: RocqQ -> Action FilePath
needRocq q = do
  (store, _) <- askStoreOracle q
  pure (store </> "bin" </> "coqc")

-- * Running Rocq
--
-- $shakeRocqCommands

-- | Default arguments to pass to @rocq@ to typecheck a file.
rocqCheckDefaultArgs :: FilePath -> [String]
rocqCheckDefaultArgs file = [file]

-- | Check that a @rocq@ installation is working by compiling an empty file.
rocqDoctor :: RocqQ -> Action ()
rocqDoctor rocqQ = do
  rocq <- needRocq rocqQ
  withTempDir \dir -> do
    let testFile = dir </> "Test.v"
    liftIO $ writeFile testFile $ unlines
      [ "Module Test."
      , "End Test."
      ]
    command_ [Cwd dir] rocq (rocqCheckDefaultArgs testFile)


-- * Shake Rules for Rocq
--
-- $shakeRocqRules

-- | Shake rules for installing @rocq@.
rocqRules :: Rules ()
rocqRules = do
  addStoreOracle "rocq" rocqInstallOracle

  withTargetDocs rocqInstallDocs $ phony "install-rocq" do
    opts <- needRocqInstallOpts
    _ <- needRocq opts
    pure ()

  phony "clean-rocq" do
    removeFilesAfter "_build/repos" ["rocq-*"]
    removeFilesAfter "_build/store" ["rocq-*"]
    pruneGitWorktrees "_build/repos/rocq"
