-- | Shake rules for compiling a particular version of @idris2@.
module Panbench.Shake.Lang.Idris
  ( -- $shakeIdrisInstall
    IdrisQ(..)
  , SchemeCompiler(..)
  , needIdris
  -- $shakeIdrisCommands
  , idrisCheckDefaultArgs
  , idrisDoctor
  -- $shakeIdrisRules
  , idrisRules
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.AllCores
import Panbench.Shake.Git
import Panbench.Shake.Make
import Panbench.Shake.Store

import System.Directory qualified as Dir
import System.FilePath

-- * Idris 2 Installation
--
-- $shakeIdrisInstall

-- | Query for installing a version of @idris2@.
data IdrisQ = IdrisQ
  { idrisInstallRev :: String
  -- ^ Revision of @idris2@ to install.
  , idrisInstallScheme :: SchemeCompiler
  -- ^ Which scheme compiler to use to install @idris2@.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Scheme compiler to use to compile @idris2@.
data SchemeCompiler
  = Chez
  -- ^ Compile @idris2@ using Chez scheme.
  | Racket
  -- ^ Compile @idris2@ using Racket.
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult IdrisQ = FilePath

-- | Oracle for installing a version of Idris 2.
idrisInstall :: IdrisQ -> FilePath -> Action ()
idrisInstall IdrisQ{..} storeDir = do
  let repoDir = "_build/repos/idris2"
  let workDir = replaceDirectory storeDir "_build/repos"
  needGitWorktree $ GitWorktreeQ
    { gitWorktreeUpstream = "https://github.com/idris-lang/Idris2.git"
    , gitWorktreeRepo = repoDir
    , gitWorktreeDir = workDir
    , gitWorktreeRev = idrisInstallRev
    }
  withAllCores \nCores -> do
    -- [FIXME: Reed M, 21/07/2025] @make bootstrap@ removes compiled lib files,
    -- which can cause some pointless rebuilds if we delete the store. We should
    -- investigate if there is a way to fix this.
    case idrisInstallScheme of
      Chez -> do
        makeCommand_ [Cwd workDir, AddEnv "SCHEME" "chez"] ["bootstrap", "-j" ++ show nCores]
        makeCommand_ [Cwd workDir, AddEnv "PREFIX" storeDir] ["install", "-j" ++ show nCores]
      Racket -> do
        makeCommand_ [Cwd workDir] ["bootstrap-racket", "-j" ++ show nCores]
        makeCommand_ [Cwd workDir, AddEnv "PREFIX" storeDir, AddEnv "IDRIS2_CG" "racket"] ["install", "-j" ++ show nCores]

-- | Require that a particular version of @idris2@ is installed,
-- and return the absolute path pointing to the executable.
needIdris :: IdrisQ -> Action FilePath
needIdris q = do
  (store, _) <- askStoreOracle q
  liftIO $ Dir.makeAbsolute (store </> "bin" </> "idris2")

-- * Running Idris
--
-- $shakeIdrisCommands

-- | Default arguments to pass to @idris2@ to check a file.
idrisCheckDefaultArgs :: FilePath -> [String]
idrisCheckDefaultArgs file = ["--check", file]

-- | Check that an @idris@ install is functioning by compiling an empty file.
idrisDoctor :: IdrisQ -> Action ()
idrisDoctor idrisQ = do
  idris <- needIdris idrisQ
  withTempDir \dir -> do
    let testFile = dir </> "Test.idr"
    liftIO $ writeFile testFile $ unlines
      [ "module Main"
      , ""
      , "main : IO ()"
      , "main = putStrLn \"\""
      ]
    command_ [Cwd dir] idris (idrisCheckDefaultArgs testFile)


-- | Shake rules for installing @idris2@.
idrisRules :: Rules ()
idrisRules = do
  addStoreOracle "idris" idrisInstall
  phony "clean-idris" do
    removeFilesAfter "_build/repos" ["idris2-*"]
    removeFilesAfter "_build/store" ["idris2-*"]
    pruneGitWorktrees "_build/repos/idris"
