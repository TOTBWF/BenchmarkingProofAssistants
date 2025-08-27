-- | Shake helpers for working with @chez@.
module Panbench.Shake.Chez
  ( -- * Locating @chez@
    ChezA(..)
  , needChez
    -- * Shake rules for @chez@
  , chezRules
  ) where

import Data.ByteString qualified as BS
import Data.Coerce
import Data.Monoid

import Development.Shake
import Development.Shake.Classes

import GHC.Generics

import Panbench.Shake.Digest

import System.Directory qualified as Dir

-- | Find a version of @chez@ on the system path.
data ChezQ = ChezQ
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

-- | Response of a 'ChezQ' query.
data ChezA = ChezA
  { chezBinPath :: FilePath
  -- ^ Absolute path of the @chez@ binary.
  , chezVersion :: String
  -- ^ Version of @chez@, as reported by @chez --version@
  , chezDigest :: BS.ByteString
  -- ^ SHA 256 hash of the @opam@ binary.
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult ChezQ = ChezA

-- | Require that @chez@ is installed.
needChez :: Action FilePath
needChez = chezBinPath <$> askOracle ChezQ

-- | Shake oracle for finding the @chez@ binary.
findChezCommandOracle :: ChezQ -> Action ChezA
findChezCommandOracle ChezQ =
  (liftIO $ coerce $ foldMap (\nm -> coerce @(IO (Maybe FilePath)) @(Ap IO (First FilePath)) (Dir.findExecutable nm)) ["chez", "chezscheme"]) >>= \case
    Nothing ->
      fail $ unlines $
        [ "Could not find a chez executable in the path"
        , "Perhaps it is not installed?"
        ]
    Just chezBinPath -> do
      Stdout chezVersion <- command [] chezBinPath ["--version"]
      chezDigest <- fileDigest chezBinPath
      pure ChezA {..}

-- | Shake rules for @chez@.
chezRules :: Rules ()
chezRules = do
    _ <- addOracleCache findChezCommandOracle
    pure ()
