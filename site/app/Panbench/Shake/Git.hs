-- | Shake utilities for interacting with @git@.
module Panbench.Shake.Git
  ( GitCloneQ(..)
  , needGitClone
  , GitWorktreeQ(..)
  , needGitWorktree
  , gitRules
  ) where

import Control.Monad

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import System.FilePath

-- | Check if a directory exists and contains a git repository.
gitRepoExists :: FilePath -> Action Bool
gitRepoExists dir =
  doesDirectoryExist (dir </> ".git")

-- | Shake query for cloning a git repository.
data GitCloneQ = GitCloneQ
  { gitCloneUpstream :: FilePath
  -- ^ URL of the repository to clone.
  , gitCloneDir :: FilePath
  -- ^ Relative path to clone the repository to.
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult GitCloneQ = ()

-- | Oracle for cloning a git repository.
gitCloneOracle :: Rules (GitCloneQ -> Action ())
gitCloneOracle =
  addOracle \GitCloneQ{..} ->
    gitRepoExists gitCloneDir >>= \case
      True -> pure ()
      False -> command_ [] "git" ["clone", gitCloneUpstream, gitCloneDir]

-- | Require that a repository is cloned.
--
-- If a git repository already exists at provided
-- path, no clone will be performed.
needGitClone :: GitCloneQ -> Action ()
needGitClone = askOracle

-- | Shake query for creating a @git@ worktree.
data GitWorktreeQ = GitWorktreeQ
  { gitWorktreeUpstream :: FilePath
  -- ^ Upstream of the main git repo.
  , gitWorktreeRepo :: FilePath
  -- ^ Relative path to the main repository of the worktree.
  , gitWorktreeDir :: FilePath
  -- ^ Relative path to create the worktree in.
  , gitWorktreeRev :: String
  -- ^ Revision to check out for the worktree.
  }

  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult GitWorktreeQ = ()

-- | Oracle for creating a git worktree.
gitWorktreeOracle :: Rules (GitWorktreeQ -> Action ())
gitWorktreeOracle =
  addOracle \GitWorktreeQ{..} -> do
    needGitClone (GitCloneQ gitWorktreeUpstream gitWorktreeRepo)
    -- Worktrees store their .git in a file, not a directory.
    doesFileExist (gitWorktreeDir </> ".git") >>= \case
      True -> pure ()
      False -> command [] "git" ["--git-dir", gitWorktreeRepo </> ".git", "worktree", "add", gitWorktreeDir, gitWorktreeRev]

-- | Require that a git worktree for a repository exists.
--
-- This will clone the repository if required.
needGitWorktree :: GitWorktreeQ -> Action ()
needGitWorktree = askOracle

gitRules :: Rules ()
gitRules = do
  _ <- gitCloneOracle
  _ <- gitWorktreeOracle
  pure ()
