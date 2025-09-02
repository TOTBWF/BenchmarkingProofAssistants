{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Panbench generators.
module Panbench.Generator
  ( GenModule(..)
  , genModuleVia
  ) where

import Data.Kind
import Data.Text (Text)

import GHC.Generics

import Panbench.Grammar
import Panbench.Pretty


-- | A generator for a module.
data GenModule (size :: Type) (m :: Type) (rep :: Type) =
  GenModule
  { genName :: Text
  -- ^ The name of the module.
  , genHeader :: m
  -- ^ Generate the module header.
  --
  -- This is used to establish baselines during benchmarking.
  , genBody :: size -> m
  -- ^ Generate the body of the module.
  }
  deriving (Generic)

-- | Generate a module, and render it as 'Text'.
genModuleVia
  :: (Module rep m)
  => (m -> Doc ann) -- ^ How to print the module into a 'Doc'.
  -> size -- ^ The module size
  -> GenModule size m rep -- ^ The generator
  -> Text
genModuleVia f size (GenModule nm header body) =
  renderVia f (moduleHeader nm <> header <> body size)
