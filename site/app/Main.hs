module Main where

import Development.Shake

import System.Directory

import Panbench.Shake.Dev
import Panbench.Shake.Env
import Panbench.Shake.Git
import Panbench.Shake.HTML
import Panbench.Shake.Lang
import Panbench.Shake.Lang.Agda
import Panbench.Shake.Lang.Idris
import Panbench.Shake.Lang.Lean
import Panbench.Shake.Lang.Rocq
import Panbench.Shake.Make
import Panbench.Shake.Matrix
import Panbench.Shake.Opam

main :: IO ()
main = shakeArgs (shakeOptions {shakeFiles="_build"}) do
  generatorRules
  benchmarkMatrixRules
  siteRules

  makeRules
  opamRules
  gitRules
  envRules

  agdaInstallRules
  leanInstallRules
  idrisInstallRules
  rocqInstallRules

  withTargetDocs "Remove all generated html files." $
    phony "clean-site" do
      removeFilesAfter "_build" ["site/*"]

  withTargetDocs "Remove all generated outputs and html files." $
    phony "clean" do
      removeFilesAfter "_build" ["agda/*", "lean/*", "idris2/*", "rocq/*", "*.html"]

  withTargetDocs "Delete the entire _build directory, including the shake database." $
    phony "clean-everything" do
      liftIO $ removeDirectoryRecursive "_build"

  withTargetDocs "Delete the build store." $
    phony "clean-store" do
      liftIO $ removeDirectoryRecursive "_build/store"

  -- Development rules
  generateCBitsClangd
