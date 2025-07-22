-- | Shake oracles that use content-addressable storage.
module Panbench.Shake.Store
  ( -- $shakeStore
    addStoreOracle
  , askStoreOracle
  -- $shakeDigest
  , fileDigest
  , directoryDigest
  , showHex
  ) where

import Control.Monad
import Control.Monad.IO.Class

import Crypto.Hash.SHA256 qualified as SHA256

import Data.Binary
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Development.Shake
import Development.Shake.Classes
import Development.Shake.Rule

import GHC.Generics
import GHC.Stack

import Panbench.Shake.File

import System.Directory qualified as Dir
import System.FilePath

-- * Content-addressed oracles
--
-- $shakeStore

newtype StoreOracleQ q = StoreOracleQ q
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, Binary, NFData)

type instance RuleResult (StoreOracleQ q) = (FilePath, BS.ByteString)

addStoreOracle
  :: forall q. (ShakeValue q, HasCallStack)
  => String
  -- ^ Name of the store entry.
  -> (q -> FilePath -> Action ())
  -- ^ Action to populate the store.
  -> Rules ()
addStoreOracle name act = do
  addBuiltinRule noLint identify run
  where
    identify :: StoreOracleQ q -> (FilePath, BS.ByteString) -> Maybe BS.ByteString
    identify _ (_, hash) = Just hash

    run :: StoreOracleQ q -> Maybe BS.ByteString -> RunMode -> Action (RunResult (FilePath, BS.ByteString))
    run (StoreOracleQ q) oldHash mode = do
      cwd <- liftIO $ Dir.getCurrentDirectory
      let storeKey = name <> "-" <> (showHex $ SHA256.hashlazy $ encode q)
      let storePath = cwd </> "_build" </> "store" </> storeKey
      (liftIO $ Dir.doesDirectoryExist storePath) >>= \case
        True -> do
          newHash <- directoryDigest storePath
          case (oldHash, mode) of
            (Just oldHash, RunDependenciesSame) | oldHash == newHash ->
              pure $ RunResult ChangedNothing oldHash (storePath, oldHash)
            _ ->
              pure $ RunResult ChangedRecomputeDiff newHash (storePath, newHash)
        False -> do
          act q storePath
          newHash <- directoryDigest storePath
          case (oldHash, mode) of
            (Just oldHash, RunDependenciesSame) | oldHash == newHash ->
              pure $ RunResult ChangedRecomputeSame oldHash (storePath, oldHash)
            _ ->
              pure $ RunResult ChangedRecomputeDiff newHash (storePath, newHash)

-- | Query the store.
askStoreOracle
  :: forall q. (ShakeValue q, HasCallStack)
  => q
  -> Action (FilePath, BS.ByteString)
askStoreOracle = apply1 . StoreOracleQ

-- * SHA-256 digests
--
-- $shakeDigest

-- | Compute the SHA-256 hash of a file.
fileDigest :: (MonadIO m) => FilePath -> m BS.ByteString
fileDigest file =
  liftIO $ SHA256.hashlazy <$> LBS.readFile file

-- | Compute the SHA-256 hash of the contents of a directory.
--
-- Note that this only computes the digest of all of the *contents* of the
-- files, and does not take directory structure into account.
directoryDigest :: (MonadIO m) => FilePath -> m BS.ByteString
directoryDigest dir = liftIO do
  paths <- getDirectoryFilesRecursive dir
  SHA256.finalize <$> foldM (\ctx path -> SHA256.update ctx <$> BS.readFile path) SHA256.init paths

-- | Show a strict bytestring as hex.
showHex :: BS.ByteString -> String
showHex bytes = do
  b <- BS.unpack bytes
  let (hi, lo) = b `divMod` 16
  [word4Hex hi, word4Hex lo]
  where

-- | Convert a @'Word8'@ in the range @0..15@ to the corresponding
-- hex character.
--
-- If the byte is greater than 15, this will throw an error.
word4Hex :: Word8 -> Char
word4Hex 0 = '0'
word4Hex 1 = '1'
word4Hex 2 = '2'
word4Hex 3 = '3'
word4Hex 4 = '4'
word4Hex 5 = '5'
word4Hex 6 = '6'
word4Hex 7 = '7'
word4Hex 8 = '8'
word4Hex 9 = '9'
word4Hex 10 = 'a'
word4Hex 11 = 'b'
word4Hex 12 = 'c'
word4Hex 13 = 'd'
word4Hex 14 = 'e'
word4Hex 15 = 'f'
word4Hex _ = error "word4Hex: expected value in range 0..15"
