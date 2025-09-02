{-# LANGUAGE GADTs #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Panbench.Shake.Matrix
  (
  -- * Benchmarking matrices
    BenchmarkMatrixRow(..)
  , benchmarkMatrixRow
  , BenchmarkMatrix(..)
  -- * Benchmark matrix statistics
  , BenchmarkMatrixStats(..)
  -- * Running benchmark matrices
  , needBenchmarkMatrix
  , needBenchmarkMatrices
  ) where

import Data.Aeson ((.:))
import Data.Aeson qualified as JSON
import Data.Foldable
import Data.Functor
import Data.Traversable

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import System.FilePath

import Panbench.Generator
import Panbench.Shake.Benchmark
import Panbench.Shake.Lang

--------------------------------------------------------------------------------
-- Benchmarking matrices

-- | A benchmarking matrix row.
data BenchmarkMatrixRow size where
  -- | Pack up a 'GenModule' along with a 'ShakeLang' dictionary.
  --
  -- Users are encouraged to use 'benchmarkMatrixRow', which takes an explicit type argument.
  BenchmarkMatrixRow :: forall m rep size. (ShakeLang m rep) => GenModule size m rep -> BenchmarkMatrixRow size


-- | Make a benchmarking matrix row.
benchmarkMatrixRow
  :: forall m size. forall rep
  -> (ShakeLang m rep)
  => GenModule size m rep
  -> BenchmarkMatrixRow size
benchmarkMatrixRow _ = BenchmarkMatrixRow

-- | A benchmarking matrix.
data BenchmarkMatrix where
  BenchmarkMatrix :: forall size. (Show size) => String -> [size] -> [BenchmarkMatrixRow size] -> BenchmarkMatrix

--------------------------------------------------------------------------------
-- Benchmarking matrix statistics

-- | Benchmarking statistics for a @'BenchmarkMatrix'@.
--
-- This is stored in a format that can easily be consumed by @vega-lite@.
newtype BenchmarkMatrixStats = BenchmarkMatrixStats [(String, String, BenchmarkExecStats)]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (Hashable, NFData)

-- | We need this somewhat annoying instance to make our data a bit more @vega-lite@
-- friendly.
instance JSON.ToJSON (BenchmarkMatrixStats) where
  toJSON (BenchmarkMatrixStats stats) =
    JSON.toJSON $ stats <&> \(lang, size, BenchmarkExecStats{..}) ->
      JSON.object
      [ ("lang", JSON.toJSON lang)
      , ("size", JSON.toJSON size)
      , ("user", JSON.toJSON benchUserTime)
      , ("system", JSON.toJSON benchSystemTime)
      , ("rss", JSON.toJSON benchMaxRss)
      , ("exit" , JSON.toJSON benchExitCode)
      ]

instance JSON.FromJSON (BenchmarkMatrixStats) where
  parseJSON =
    JSON.withArray "BenchmarkMatrixStats" \objs -> do
    entries <-
      for objs $ JSON.withObject "BenchmarkMatrixStat" \obj -> do
        lang <- obj .: "lang"
        size <- obj .: "size"
        benchUserTime <- obj .: "user"
        benchSystemTime <- obj .: "system"
        benchMaxRss <- obj .: "rss"
        benchExitCode <- obj .: "exit"
        pure (lang, size, BenchmarkExecStats {..})
    pure $ BenchmarkMatrixStats $ toList $ entries

--------------------------------------------------------------------------------
-- Running benchmark matrices

needBenchmarkMatrix
  :: BenchmarkMatrix
  -> Action BenchmarkMatrixStats
needBenchmarkMatrix (BenchmarkMatrix _ sizes rows) = BenchmarkMatrixStats <$>
  for (liftA2 (,) sizes rows) \(size, BenchmarkMatrixRow @_ @rep gen) -> do
    bin <- needLang rep
    (dir, file) <- splitFileName <$> needModule gen size
    cleanBuildArtifacts rep dir
    stat <- liftIO $ benchmark bin (defaultCheckArgs rep file) [("HOME", dir)] dir
    pure (langName rep, show size, stat)

needBenchmarkMatrices :: [BenchmarkMatrix] -> Action [BenchmarkMatrixStats]
needBenchmarkMatrices = traverse needBenchmarkMatrix
