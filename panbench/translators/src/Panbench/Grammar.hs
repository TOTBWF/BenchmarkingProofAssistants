{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- | Tagless grammar for panbench.
module Panbench.Grammar
  (
  -- * Names
    Name
  , name
  , names
  -- * Binders
  , Visibility(..)
  , Arg(..)
  -- * Terms
  , LetDef(..)
  , Term(..)
  -- * Patterns
  , Pattern(..)
  , Clause(..)
  -- * Definitions
  , Constr(..)
  , Module(..)
  -- * Operators and Builtins
  , Builtin(..)
  , builtin
  , Op1
  , op1
  , Op2
  , op2
  -- * Imports
  , Import(..)
  , import_
  -- * Helpers
  , vars
  , lets_
  ) where

import Numeric.Natural

import Data.List.NonEmpty (NonEmpty)
import Data.Kind
import Data.Text (Text)
import Data.Text qualified as T

import GHC.TypeLits

import Panbench.Data.List.Bwd (Bwd)
import Panbench.Data.List.Bwd qualified as Bwd

--------------------------------------------------------------------------------
-- Names

type Name = Text

name :: Name -> Natural -> Name
name x n = x <> T.pack (show n)

names :: Name -> [Natural] -> [Name]
names x ns = name x <$> ns

--------------------------------------------------------------------------------
-- Binders

-- | Binder visibility
data Visibility
  = Explicit
  -- ^ An explicit binder like @(x : A) -> B x@
  | Implicit
  -- ^ An implicit binder like @{x : A} -> B x@

-- | Binder metadata.
data Arg nm arg = Arg { argNm :: nm, unArg :: arg , argVis :: Visibility}

--------------------------------------------------------------------------------
-- Terms

data LetDef rep
  = ChkLetDef Name rep [Pattern rep] rep
  | SynLetDef Name [Pattern rep] rep

-- | Terms.
class Term (rep :: Type) where
  -- | Variables.
  var :: Name -> rep

  -- | Non-dependent function types.
  --
  -- We differentiate between dependent and non-dependent function types
  -- to get nicer pretty-printing.
  arr :: rep -> rep -> rep

  -- | Dependent function types.
  pi :: NonEmpty (Arg [Name] rep) -> rep -> rep

  -- | N-ary application.
  --
  -- Using n-ary application allows for nicer display.
  app :: rep -> [rep] -> rep

  let_ :: [LetDef rep] -> rep -> rep

  -- | The universe.
  univ :: rep

  -- | Explicit parentheses.
  parens :: rep -> rep


--------------------------------------------------------------------------------
-- Operators and Builtins

class (KnownSymbol op) => Builtin (rep :: Type) (op :: Symbol) (tp :: Type) | rep op -> tp, op tp -> rep where
  mkBuiltin :: tp

builtin :: forall rep tp. forall op -> (Builtin rep op tp) => tp
builtin o = mkBuiltin @rep @o @tp

type Op1 rep op = Builtin rep op (rep -> rep)

-- | Shorthand for a unary operator.
op1 :: forall op -> Op1 rep op => rep -> rep
op1 = builtin

type Op2 rep op = Builtin rep op (rep -> rep -> rep)

op2 :: forall op -> Op2 rep op => rep -> rep -> rep
op2 = builtin


--------------------------------------------------------------------------------
-- Patterns

data Pattern rep
  = ConPat Visibility Name [Pattern rep]
  | VarPat Visibility Name


data Clause rep = Clause { clausePats :: [Pattern rep], clauseBody :: rep }

--------------------------------------------------------------------------------
-- Definitions

data Constr rep = Constr { constrName :: Name, constrTp :: rep }

class (Term rep, Monoid m) => Module (rep :: Type) (m :: Type) | rep -> m, m -> rep where
  -- | Module of a (possibly nullary) function.
  defFn     :: Name -> rep -> rep -> m

  -- | Module via pattern matching.
  defMatch :: Name -> rep -> [Clause rep] -> m

  -- | Define a datatype.
  defData
    :: Name -- ^ The name of the datatype.
    -> [Arg [Name] rep] -- ^ Datatype parameters.
    -> [Constr rep] -- ^ Datatype constructors.
    -> m

--------------------------------------------------------------------------------
-- Imports

class (KnownSymbol i) => Import (m :: Type) (i :: Symbol) where
  mkImport :: m

import_ :: forall i -> (Import m i) => m
import_ i = mkImport @_ @i

--------------------------------------------------------------------------------
-- Helpers

-- | Construct a list of variables @x_i, x_j, x_k@ from a base name
-- and a list of numeric subscripts.
vars :: (Term rep) => Name -> [Natural] -> [rep]
vars x ns = var <$> names x ns

-- | HOAS form of a sequence of let bindings.
lets_ :: (Term rep) => [(Name, rep)] -> (Bwd rep -> rep) -> rep
lets_ = Bwd.scoped (\(x, e) -> let_ [SynLetDef x [] e]) (\(x, _) -> var x)
