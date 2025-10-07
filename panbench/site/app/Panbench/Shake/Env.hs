-- | Shake utilities for interacting with environment variables.
module Panbench.Shake.Env
  ( -- * Path-related queries
    askPath
  , diffPathPrefix
  , diffPathSuffix
    -- * Environment variables
  , askEnvironment
    -- * Shake Rules
  , envRules
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import System.Environment qualified as Env
import System.FilePath

--------------------------------------------------------------------------------
-- Path-related queries

-- | Shake query for getting the path.
data PathQ = PathQ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult PathQ = [String]

-- | Get the current value of $PATH.
askPath :: Action [String]
askPath = askOracle PathQ

-- | @diffPathPrefix new old@ returns a list of @$PATH@ entries
-- that were newly added to the front of @$PATH@.
diffPathPrefix :: [String] -> [String] -> [String]
diffPathPrefix [] _ = []
diffPathPrefix news [] = news
diffPathPrefix (new:news) (old:olds)
  | new == old = []
  | otherwise = new:(diffPathPrefix news (old:olds))

-- | @diffPathPrefix new old@ returns a list of @$PATH@ entries
-- that were newly added to the back of @$PATH@.
diffPathSuffix :: [String] -> [String] -> [String]
diffPathSuffix news olds = reverse (diffPathPrefix (reverse news) (reverse olds))

--------------------------------------------------------------------------------
-- Environment variables

-- | Shake query for getting environment variables.
data EnvQ = EnvQ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult EnvQ = [(String, String)]

-- | Get all environment variables.
askEnvironment :: Action [(String, String)]
askEnvironment = askOracle EnvQ

--------------------------------------------------------------------------------
-- Shake rules

envRules :: Rules ()
envRules = do
  _ <- addOracleHash \PathQ -> liftIO do
    path <- Env.getEnv "PATH"
    pure $ splitSearchPath path
  _ <- addOracleHash \EnvQ -> liftIO $ Env.getEnvironment
  pure ()
