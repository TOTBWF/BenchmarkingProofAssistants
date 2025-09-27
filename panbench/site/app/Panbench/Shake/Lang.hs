{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | @shake@ build rules for @panbench@ modules.
module Panbench.Shake.Lang
  ( -- * Shake rules for languages
    ShakeLang(..)
  , generatorOutputDir
  , SomeLangModule(..)
  , needModules
  ) where

import Data.Text qualified as T
import Data.Traversable

import Development.Shake

import Panbench.Generator
import Panbench.Grammar
import Panbench.Grammar.Agda
import Panbench.Shake.File
import Panbench.Shake.Lang.Agda

import System.FilePath

-- | Rules for installing, building, and cleaning files
-- for a given language
class (Module m hdr defn) => ShakeLang m hdr defn rep | m -> rep, rep -> m where
  -- | User-facing name of the language.
  langName :: forall rep' -> (rep ~ rep') => String

  -- | File extension of a language
  langExt :: forall rep' -> (rep ~ rep') => String

  -- | Require that the language is installed.
  --
  -- We use @forall rep' -> (rep ~ rep') => Action FilePath@
  -- so that 'needLang' can be called with an explicit type
  -- argument like @needLang Agda@.
  needLang :: forall rep' -> (rep ~ rep') => Action FilePath

  -- | Require that a module described by a 'Gen' get built.
  -- This should return an absolute filepath.
  needModule :: (Show size) => GenModule size hdr defn -> size -> Action FilePath

  -- | Clean all build artifacts for a language in the given directory.
  cleanBuildArtifacts :: forall rep' -> (rep ~ rep') => FilePath -> Action ()

  -- | Get the default arguments used to check a file for a language.
  defaultCheckArgs :: forall rep' -> (rep ~ rep') => FilePath -> [String]

-- | Existential for 'GenModule' that packs up evidence that we
-- actually know how generate and typecheck the module.
data SomeLangModule where
  -- | Pack a 'GenModule' alongside a 'ShakeLang' dictionary.
  SomeLangModule :: forall m hdr defn rep size. (ShakeLang m hdr defn rep, Show size) => GenModule size hdr defn -> size -> SomeLangModule

-- | Get the output file for a module in a standardized format.
--
-- We will use the following convention for paths:
--
-- > _build/lang/gen/n/mod.ext
--
-- Where @lang@ is the language name, @gen@ is the name of the generator,
-- @n@ is the size parameter, and @mod.ext@ is the rendered module file.
generatorOutputDir
  :: String -- ^ Language name
  -> String -- ^ Module name
  -> String -- ^ Size
  -> String -- ^ Extension
  -> FilePath
generatorOutputDir lang nm size ext =
  "_build" </> lang </> nm </> size </> nm <.> ext

-- | Request that a list of modules be generated.
--
-- This query is subject to caching.
needModules :: [SomeLangModule] -> Action [(FilePath, FilePath)]
needModules gens =
  -- Can't use a variant of 'asks' here for type reasons.
  for gens \(SomeLangModule gen size) -> do
    path <- needModule gen size
    pure (splitFileName path)

--------------------------------------------------------------------------------
-- Instances

instance ShakeLang (AgdaMod ann) (AgdaHeader ann) (AgdaDefn ann) (Agda ann) where
  langName _ = "agda"
  langExt _ = ".agda"
  needLang _ = do
    opts <- needAgdaInstallOpts
    needAgda opts
  needModule gen size = do
    let path = generatorOutputDir "agda" (T.unpack (genName gen)) (show size) ".agda"
    writeTextFileChanged path (genModuleVia getAgda size gen)
    pure path
  defaultCheckArgs _ = agdaCheckDefaultArgs
  cleanBuildArtifacts _ dir = removeFilesAfter dir ["*.agdai"]
