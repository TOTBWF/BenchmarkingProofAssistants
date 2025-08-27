-- | Shake utilities for interacting with environment variables.
module Panbench.Shake.Env
  ( -- $shakePath
    askPath
  , diffPathPrefix
  , diffPathSuffix
  -- $shakePathRules
  , envRules
  ) where

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import System.Environment qualified as Env
import System.FilePath

-- * Path-related queries
--
-- $shakePath

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
diffPathPrefix [] olds = []
diffPathPrefix news [] = news
diffPathPrefix (new:news) (old:olds)
  | new == old = []
  | otherwise = new:(diffPathPrefix news (old:olds))

-- | @diffPathPrefix new old@ returns a list of @$PATH@ entries
-- that were newly added to the back of @$PATH@.
diffPathSuffix :: [String] -> [String] -> [String]
diffPathSuffix news olds = reverse (diffPathPrefix (reverse news) (reverse olds))

-- * Shake Rules
--
-- $shakePathRules

envRules :: Rules ()
envRules = do
  _ <- addOracle \PathQ -> liftIO do
    path <- Env.getEnv "PATH"
    pure $ splitSearchPath path
  pure ()
