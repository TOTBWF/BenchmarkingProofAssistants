-- | Golden tests.
module Main where

import Data.Text.IO.Utf8 as Utf8
import Data.Text.Encoding as Utf8
import Data.Text as T
import Data.ByteString.Lazy as LBS

import Panbench.Grammar.Agda

import Panbench.Generator

import Panbench.Generator.DatatypeParameters qualified as DatatypeParameters
import Panbench.Generator.LargeDependentRecord qualified as LargeDependentRecord
import Panbench.Generator.LargeIndexedDatatype qualified as LargeIndexedDatatype
import Panbench.Generator.NestedLet qualified as NestedLet
import Panbench.Generator.NestedLetAdditions qualified as NestedLetAdditions
import Panbench.Generator.NestedLetFunctions qualified as NestedLetFunctions

import System.Directory
import System.FilePath

import Test.Tasty.Golden
import Test.Tasty

import Golden.Util.File

-- import Grammar
-- import Tests

-- * Testing utilities

-- | Convert a filename to a path to a snapshot file.
--
-- >>> snapshotPath "LetExample.agda"
-- "test/snapshots/LetExample.agda"
snapshotPath
  :: String
  -- ^ The base name to use for the snapshot file.
  -> FilePath
snapshotPath fname = "test" </> "snapshot" </> fname

-- | Convert a filename to a path to a staging file.
--
-- >>> stagingPath "LetExample.agda"
-- "test/staging/LetExample.agda"
stagingPath
  :: String
  -- ^ The name to use for the staging file.
  -> FilePath
stagingPath fname = "test" </> "staging" </> fname

-- | Create a golden test printing for a language.
--   The resulting snapshot files will always be encoded
--   using UTF-8. Moreover, no newline conversion will
--   be performed, and the users locale will be ignored.
printTestForLang
  :: String
  -- ^ The name of the language.
  -> (a -> Text)
  -- ^ The printer to use for this thing.
  -> String
  -- ^ The extension to use for the saved snapshot and staging files.
  -> String
  -- ^ The base name to use for snapshot and staging files.
  -> a
  -- ^ The thing to print.
  -> TestTree
printTestForLang langName printer fileExt base syn =
  -- We have to use @goldenVsStringDiff@ ourselves to avoid bad unicode decoding...
  goldenVsStringDiff langName (\ref new -> ["diff", "--strip-trailing-cr" ,"-u", "--color=always", ref, new]) snapshotFile do
    createDirectoryIfMissing False ("test" </> "staging")
    createFile stagingFile
    -- Data.Text.IO.Utf8 always writes UTF-8, ignores the locale,
    -- and does not do line ending conversion.
    let result = printer syn
    Utf8.writeFile stagingFile result
    pure (LBS.fromStrict $ Utf8.encodeUtf8 result)
  where
    stagingFile = stagingPath (base <.> fileExt)
    snapshotFile = snapshotPath (base <.> fileExt)

agdaModuleTest
  :: GenModule size (AgdaHeader ()) (AgdaDefn ())
  -> size
  -> TestTree
agdaModuleTest gen size =
  printTestForLang "agda" (genModuleVia getAgda size) ".agda" (T.unpack (genName gen)) gen

-- * Tests
--
-- These tests are all taken from the original @Tests.hs@ file,
-- and the size parameters were reverse engineered from @good_Output@.
-- All files in tests/snapshots originally also originated from @good_Output@,
-- so there is a throughline.

main :: IO ()
main = defaultMain $
  testGroup "Golden"
  [ testGroup "Agda"
    [ agdaModuleTest DatatypeParameters.generator 5
    , agdaModuleTest LargeDependentRecord.generator 5
    , agdaModuleTest LargeIndexedDatatype.generator 5
    , agdaModuleTest NestedLet.generator 5
    , agdaModuleTest NestedLetAdditions.generator 5
    , agdaModuleTest NestedLetFunctions.generator 5
    ]
  ]
