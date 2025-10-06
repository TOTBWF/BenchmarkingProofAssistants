-- | Shake utilities for interacting with @git@.
module Panbench.Shake.Git
  ( gitRepoExists
  , gitWorktreeExists
  -- $gitClone
  , GitCloneQ(..)
  , needGitClone
  -- $gitWorktree
  , GitWorktreeQ(..)
  , pruneGitWorktrees
  , removeGitWorktree
  , withGitWorktree
  -- $gitRules
  , gitRules
  ) where

import Control.Monad.IO.Class

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import System.FilePath
import System.Directory qualified as Dir
import System.Process qualified as Proc

-- | Check if a directory exists and contains a git repository.
--
-- Unlike @'doesFileExist'@, this does not add the directory as a
-- @shake@ dependency.
gitRepoExists :: (MonadIO m) => FilePath -> m Bool
gitRepoExists dir =
  -- We use @Dir.doesDirectoryExist@ to avoid tracking the @.git@ folder as a dep.
  liftIO $ Dir.doesDirectoryExist (dir </> ".git")

-- | Check if a worktree exists and contains a git repository.
--
-- Unlike @'doesFileExist'@, this does not add the directory as a
-- @shake@ dependency.
gitWorktreeExists :: (MonadIO m) => FilePath -> m Bool
gitWorktreeExists dir =
  -- We use @Dir.doesDirectoryExist@ to avoid tracking the @.git@ folder as a dep.
  liftIO $ Dir.doesFileExist (dir </> ".git")


-- * Git Clone
--
-- $gitClone

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

-- * Git Worktrees
--
-- $gitWorktree

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

-- | Prune all worktrees in a git repo.
--
-- If the git repo does not exist, @'pruneGitWorktrees'@ is a no-op.
pruneGitWorktrees :: (MonadIO m) => FilePath -> m ()
pruneGitWorktrees repo =
  gitRepoExists repo >>= \case
    True -> liftIO $ Proc.callProcess "git" ["--git-dir", repo </> ".git", "worktree", "prune"]
    False -> pure ()

-- | Add a git worktree.
--
-- If the git repo does not exist, throw an exception.
addGitWorktree :: (MonadFail m, MonadIO m) => GitWorktreeQ -> m ()
addGitWorktree GitWorktreeQ{..} =
  gitRepoExists gitWorktreeRepo >>= \case
    True ->
      liftIO $ Proc.callProcess "git"
        ["--git-dir", gitWorktreeRepo </> ".git"
        , "worktree", "add", "-f", gitWorktreeDir, gitWorktreeRev
        -- We detach to let ourselves check out multiple versions of the same worktree.
        , "--detach"
        ]
    False -> fail $ "addGitWorktree: Git repository " <> gitWorktreeRepo <> " does not exist."

-- | Remove a git worktree.
--
-- If the git repo does not exist, @'removeGitWorktree'@ is a no-op.
removeGitWorktree :: (MonadIO m) => GitWorktreeQ -> m ()
removeGitWorktree GitWorktreeQ{..} = do
  gitRepoExists gitWorktreeRepo >>= \case
    True ->
      liftIO $ Proc.callProcess "git"
        ["--git-dir", gitWorktreeRepo </> ".git"
        , "worktree", "remove", "-f", gitWorktreeDir
        ]
    False -> pure ()

-- | Run an action with a fresh git worktree created.
--
-- The worktree is created before the action is run, and removed afterwards.
-- If the action throws an exception (sync or async), then the worktree will
-- still be removed.
withGitWorktree :: GitWorktreeQ -> Action a -> Action a
withGitWorktree worktree@GitWorktreeQ{..} act = do
  needGitClone (GitCloneQ gitWorktreeUpstream gitWorktreeRepo)
  actionBracket
    (addGitWorktree worktree)
    (\_ -> removeGitWorktree worktree)
    (\_ -> act)

-- | Shake rules for git
--
-- $gitRules

gitRules :: Rules ()
gitRules = do
  _ <- gitCloneOracle
  pure ()
