{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
-- | Language metadata.
--
-- This module is intended to be imported qualified.
module Panbench.Lang
  (  -- * Languages
    Lang(..)
  , name
  , fileExt
  , buildArtifacts
  , allLangs
  ) where

import Control.DeepSeq

import Data.Aeson qualified as JSON
import Data.Binary
import Data.Hashable
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)


import GHC.Generics

import Text.ParserCombinators.ReadP qualified as Read
import Text.Read

import Panbench.Grammar hiding (name)
import Panbench.Pretty

-- | Possible languages to generate.
data Lang = Agda | Idris | Lean | Rocq
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (Hashable, Binary, NFData)

instance Read Lang where
  readPrec = lift $ Read.choice
    [ Agda <$ Read.string "agda"
    , Idris <$ Read.string "idris2"
    , Lean <$ Read.string "lean"
    , Rocq <$ Read.string "rocq"
    ]

instance JSON.ToJSON Lang where
  toJSON Agda = JSON.String "agda"
  toJSON Idris = JSON.String "idris2"
  toJSON Lean = JSON.String "lean"
  toJSON Rocq = JSON.String "rocq"

instance JSON.FromJSON Lang where
  parseJSON = JSON.withText "Lang" \case
    "agda" -> pure Agda
    "idris2" -> pure Idris
    "lean" -> pure Lean
    "rocq" -> pure Rocq
    _ -> fail "Expected one of agda, idris2, lean, rocq."

-- | Get the name of a language.
name :: Lang -> String
name Agda = "agda"
name Idris = "idris2"
name Lean = "lean"
name Rocq = "rocq"

-- | Get the extension type for a given @Lang@.
fileExt :: Lang -> String
fileExt Agda = ".agda"
fileExt Idris = ".idr"
fileExt Lean = ".lean"
fileExt Rocq = ".v"

-- | Build artifacts produced by a given @'Lang'@.
buildArtifacts :: Lang -> [FilePath]
buildArtifacts Agda = ["*.agdai"]
buildArtifacts Idris = ["build/*"]
buildArtifacts Lean = []
buildArtifacts Rocq = ["*.vo", "*.vok", "*.vos", "*.glob"]

-- | A @'Set'@ containing all currently supported languages.
allLangs :: Set Lang
allLangs =
  Set.fromList [minBound..maxBound]
