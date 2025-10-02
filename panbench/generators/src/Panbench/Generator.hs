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

import Data.Text (Text)

import GHC.Generics

import Panbench.Grammar
import Panbench.Pretty


-- | A generator for a module.
data GenModule size hdr defns =
  GenModule
  { genName :: Text
  -- ^ The name of the module.
  , genHeader :: [hdr]
  -- ^ Generate the module header.
  --
  -- This is used to establish baselines during benchmarking.
  , genBody :: size -> [defns]
  -- ^ Generate the body of the module.
  }
  deriving (Generic)

-- | Generate a module, and render it as 'Text'.
genModuleVia
  :: (Module mod hdr body)
  => (mod -> Doc ann) -- ^ How to print the module into a 'Doc'.
  -> size -- ^ The module size.
  -> GenModule size hdr body -- ^ The generator.
  -> Text
genModuleVia f size (GenModule nm header body) =
  renderVia f (module_ nm (mconcat header) (mconcat (body size)))
