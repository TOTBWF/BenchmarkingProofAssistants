{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleContexts #-}

module Print.Generic where

import Data.Coerce
import qualified Prettyprinter as P
import Numeric.Natural (Natural)

import Grammar (Visibility)

-- Classes for Keywoards
class Keywords rep where
  import_ :: rep
  assign  :: rep
  recrd   :: rep
  univ    :: rep
  data_   :: rep
  arr     :: rep
  lcons   :: rep
  vcons   :: rep
  typesep :: rep
  natT    :: rep
  strT    :: rep
  vectT   :: rep

-- Generic printing patterns
-- type annotations
class Keywords rep => TypeAnn rep where
  typeAnn :: rep -> rep -> rep
  teleCell :: Visibility -> rep -> rep -> rep

-- Lifting Prettyprinter functions to HasDoc
-- These should not be exported.
l1 :: forall doc ann. (Coercible (P.Doc ann) (doc ann)) => (P.Doc ann -> P.Doc ann) -> doc ann -> doc ann
l1 f x = coerce (f (coerce x))

l1' :: forall a doc ann. (Coercible (P.Doc ann) (doc ann), Coercible (doc ann) (P.Doc ann)) =>
  (a -> P.Doc ann -> P.Doc ann) -> a -> doc ann -> doc ann
l1' f a = l1 (f a)

l2 :: forall doc ann. (Coercible (P.Doc ann) (doc ann)) =>
  (P.Doc ann -> P.Doc ann -> P.Doc ann) -> doc ann -> doc ann -> doc ann
l2 f x y = coerce (f (coerce x) (coerce y))

ll :: forall doc ann. (Coercible (P.Doc ann) (doc ann)) => ([P.Doc ann] -> P.Doc ann) -> [doc ann] -> doc ann
ll f xs = coerce (f (map coerce xs))

-- Export
-- 0-ary
hardline, line, softline, softline', emptyDoc, space, lbracket, rbracket, lbrace, rbrace, dquote, comma, pipe,
  semi, dot :: forall doc ann. (Coercible (P.Doc ann) (doc ann)) => doc ann
hardline  = coerce (P.hardline @ann)
line      = coerce (P.line @ann)
softline  = coerce (P.softline @ann)
softline' = coerce (P.softline' @ann)
emptyDoc  = coerce (P.emptyDoc @ann)
space     = coerce (P.space @ann)
lbracket  = coerce (P.lbracket @ann)
rbracket  = coerce (P.rbracket @ann)
lbrace    = coerce (P.lbrace @ann)
rbrace    = coerce (P.rbrace @ann)
dquote    = coerce (P.dquote @ann)
comma     = coerce (P.comma @ann)
pipe      = coerce (P.pipe @ann)
semi      = coerce (P.semi @ann)
dot       = coerce (P.dot @ann)

-- 1-ary
align, parens, dquotes, braces, group, brackets
  :: forall doc ann. (Coercible (P.Doc ann) (doc ann)) => doc ann -> doc ann
align    = l1 P.align
parens   = l1 P.parens
dquotes  = l1 P.dquotes
braces   = l1 P.braces
group    = l1 P.group
brackets = l1 P.brackets

-- special 1-ary case
pretty :: forall p doc ann. (Coercible (P.Doc ann) (doc ann) , P.Pretty p) => p -> doc ann
pretty t = coerce @(P.Doc ann) (P.pretty t)

-- parametrized 1-ary
nest, indent :: forall doc ann. (Coercible (P.Doc ann) (doc ann)) => Int -> doc ann -> doc ann
nest   = l1' P.nest
indent = l1' P.indent

-- 2-ary
(<+>) :: Coercible (P.Doc ann) (doc ann) => doc ann -> doc ann -> doc ann
(<+>) = l2 (P.<+>)

-- list-ary
vcat, vsep, hcat, hsep, sep, fillSep :: Coercible (P.Doc ann) (doc ann) => [doc ann] -> doc ann
vcat    = ll P.vcat
vsep    = ll P.vsep
hcat    = ll P.hcat
hsep    = ll P.hsep
sep     = ll P.sep
fillSep = ll P.fillSep

-- special
punctuate :: forall doc ann. Coercible (P.Doc ann) (doc ann) => doc ann -> [doc ann] -> [doc ann]
punctuate c l = map (coerce @(P.Doc ann)) $ P.punctuate (coerce c) (map coerce l)

encloseSep :: forall doc ann. Coercible (P.Doc ann) (doc ann) =>
  doc ann -> doc ann -> doc ann -> [doc ann] -> doc ann
encloseSep a b c l = coerce @(P.Doc ann) $ P.encloseSep (coerce a) (coerce b) (coerce c) (map coerce l)

blanklines :: forall doc ann. Coercible (P.Doc ann) (doc ann) => Natural -> doc ann
blanklines n = coerce @(P.Doc ann) $ P.vcat $ replicate (fromIntegral n) P.emptyDoc

concatWith :: forall doc ann. Coercible (P.Doc ann) (doc ann) =>
  (doc ann -> doc ann -> doc ann) -> [doc ann] -> doc ann
concatWith f l = coerce @(P.Doc ann) $ P.concatWith (\ x y -> coerce $ f (coerce x) (coerce y)) (map coerce l)

-- Utilities to be shared amongst all Print modules
prettyArgs :: (P.Pretty p, Coercible (P.Doc ann) (doc ann)) => p -> (a -> doc ann) -> [a] -> doc ann
prettyArgs var pr args = if null args then pretty var else pretty var <+> hsep (map pr args)
