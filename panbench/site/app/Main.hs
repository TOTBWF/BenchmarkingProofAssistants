{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
module Main where

import Development.Shake

import Numeric.Natural

import System.Directory

import Panbench.Grammar.Agda

import Panbench.Shake.Dev
import Panbench.Shake.Chez
import Panbench.Shake.Env
import Panbench.Shake.Git
import Panbench.Shake.HTML
import Panbench.Shake.Lang.Agda
import Panbench.Shake.Lang.Idris
import Panbench.Shake.Lang.Lean
import Panbench.Shake.Lang.Rocq
import Panbench.Shake.Make
import Panbench.Shake.Matrix
import Panbench.Shake.Opam

import Panbench.Generator.NestedLetAdditions qualified as NestedLetAdditions

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  needSite <- siteRules
  "_build/site/index.html" %> \out -> do
    needSite out
      [ BenchmarkMatrix "LetAddExample" [2^n | (n :: Natural) <- [0..4]]
        [ benchmarkMatrixRow (Agda String) NestedLetAdditions.generator
        ]
      ]

  chezRules
  envRules
  gitRules
  makeRules
  opamRules

  agdaRules
  idrisRules
  leanRules
  rocqRules

  withTargetDocs "Remove all generated html files." $
    phony "clean-site" do
      removeFilesAfter "_build" ["site/*"]

  withTargetDocs "Remove all generated outputs and html files." $
    phony "clean" do
      need ["clean-site"]
      removeFilesAfter "_build" ["agda/*", "lean/*", "idris2/*", "rocq/*", "*.html"]

  withTargetDocs "Delete the entire _build directory, including the shake database." $
    phony "clean-everything" do
      liftIO $ removeDirectoryRecursive "_build"

  withTargetDocs "Delete the build store." $
    phony "clean-store" do
      liftIO $ removeDirectoryRecursive "_build/store"

  -- Development rules
  generateCBitsClangd
