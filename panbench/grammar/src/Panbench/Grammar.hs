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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
-- | Tagless grammar for panbench.
module Panbench.Grammar where
  -- (
  -- -- * Names
  --   Name
  -- , name
  -- , names
  -- -- * Binders
  -- , Visibility(..)
  -- -- , Arg(..)
  -- -- , Tele(..)
  -- , Defn(..)
  -- , Binding(..)
  -- -- * Terms
  -- , Var(..)
  -- , Pi(..)
  -- , Let(..)
  -- , Lets(..)
  -- -- * Patterns
  -- , Pattern(..)
  -- , pattern VarPats
  -- , Clause(..)
  -- -- * Definitions
  -- , Constr(..)
  -- , Module(..)
  -- -- * Operators, Builtins, and literals
  -- , Builtin(..) , builtin
  -- -- ** Builtin shorthands
  -- , Constant , constant
  -- , Op1 , op1
  -- , Op2 , op2
  -- , Literal(..) , lit
  -- , nat, list, vec, string
  -- -- * Imports
  -- , Import(..) , import_
  -- -- * Helpers
  -- , vars
  -- , varN
  -- ) where

import Prelude hiding (pi)

import Numeric.Natural

import Data.List.NonEmpty (NonEmpty)
import Data.Kind
import Data.Text (Text)
import Data.Text qualified as T
import Data.String (IsString(..))

import GHC.Exts (IsList(..))
import GHC.TypeLits

--------------------------------------------------------------------------------
-- Binders.

-- $binders
--
-- Our metalanguage needs to support lots of different binding constructs.
-- Moreover, all the languages tend to have different overlapping supported
-- sets of binding constructs. To handle this, we introduce a general langauge
-- of *binding shapes* and *binding modifiers* that act on said shapes.
--
-- As the name suggests binding shape is a general pattern of binding.
-- We support 3 different options
--
-- 1. Single binders like (x : A)
-- 2. Multi-name binders like (x y : A)
-- 3. Telescopic binders like (x y : A) (b : B) (z : C x b)
--
-- Consumers of the binder shape API are expected to use the same @binder@ type for all
-- binding-shape classes, as in the following example:
--
-- @
-- test
--   :: (Binder nm rep binder, Binders nm rep binder, Pi binder rep)
--   => rep -> rep -> rep -> rep
-- test foo bar baz = pi ("x" .: foo) $ pi (["a", "b"] .:* bar) baz
-- @
--
-- Producers of the binding shape API are expected to use the extra degree of freedom
-- provided by classes like 'Pi' to delay rendering decisions for binding constructs
-- until the use-site: see $bindingModifiers for more information.
--
-- We also add an intermediate notion of a binder "cell", which represents a
--

class (IsString nm) => Name nm where
  nameN :: nm -> Natural -> nm

class Chk nm rep cell | cell -> nm, cell -> rep where
  chk :: nm -> rep -> cell

class Chks nm rep cell | cell -> nm, cell -> rep where
  chks :: [nm] -> rep -> cell

class Syn nm cell | cell -> nm where
  syn :: nm -> cell

class Syns nm cell | cell -> nm where
  syns :: [nm] -> cell

--------------------------------------------------------------------------------
-- Deriving-via helpers

newtype SingletonCell cell = SingletonCell cell

instance Syns nm cell => Syn nm (SingletonCell cell) where
  syn nm = SingletonCell (syns [nm])

instance Chks nm rep cell => Chk nm rep (SingletonCell cell) where
  chk nm rep = SingletonCell (chks [nm] rep)

-- | Infix operator for 'chkCell'.
(.:) :: (Chk nm rep cell) => nm -> rep -> cell
(.:) = chk

-- | Infix operator for 'chksCell'.
(.:*) :: (Chks nm rep cell) => [nm] -> rep -> cell
(.:*) = chks

class Binder cell binder | binder -> cell where
  -- | A single binder like @(x : A)@.
  --
  -- By default, the binder should be a visible binder.
  binderCell :: cell -> binder

class Binders cell binder | binder -> cell where
  -- | Turn a telescope of binder cells into a single binder.
  --
  -- By default, the binder should be a visible binder.
  -- Any binding actions should act upon the *entire* binding telescope.
  binderCells :: [cell] -> binder

-- | If a language does support telescopic binders, @cell@, it is convienent to use
-- @[cell]@ as our representation of a multi-binder.
instance Binders cell [cell] where
  binderCells = id

--------------------------------------------------------------------------------
-- Binder modifiers

-- $bindingModifiers
--
-- The other half of the binder API is *binding modifiers, which handle things
-- like implicit arguments, class arguments, erasure, etc. Every sort of modifier
-- has a class associated to it like 'Implicit', which provide an (possibly parameterized)
-- action @binder -> binder@.
--
-- On the user-facing side, writing a modified binder is as easy as
--
-- @
-- pi (implicit ("x" .: nat)) nat
-- @
--
-- Instances for @Binder@ and related classes will then have to delay the
-- decision on how to fully render the binder, which is why we provide the
-- extra degree of freedom in classes like 'Pi'.

class Implicit cell where
  -- | Mark a binder cell as implicit.
  implicit :: cell -> cell

class SemiImplicit cell where
  -- | Mark a binder cell as semi-implicit
  semiImplicit :: cell -> cell

--------------------------------------------------------------------------------
-- Definitions

class LetDef lhs rep defn | defn -> rep, defn -> lhs where
  -- | A single, annotated local definition.
  letDef :: lhs -> rep -> defn

class LetDefs defn where
  -- | Create a (non-mutually recursive) group of local definitions.
  letDefs :: [defn] -> defn

--------------------------------------------------------------------------------
-- Left-hand sides

-- $leftHandSides
--
-- We need to handle left-hand sides of definitions in two places:
--
-- 1. Local let bindings
-- 2. Top-level bindings
--
-- Both of these cases allow for named definitions, anonymous definitions,
-- annotated definitions, definitions that take arguments, and (sometimes) pattern
-- definitions.
--
-- To handle this zoo of features, we break down a LHS into the following components:
--
-- A *LHS cell* is the basic building block of an LHS. These can come either annotated
-- or unannotated, and are often equipped with actions of our visibility classes.

-- | Construct a left-hand side from a single LHS cell.
class SimpleLhs cell lhs | lhs -> cell where
  lhsCell :: cell -> lhs

-- | Construct a LHS that is parameterised by a telescope of (possibly annotated)
-- arguments.
--
-- The intuition for the order of arguments is that we want to think
-- of a LHS like
--
-- @
-- let foo (A : Type) (x : Nat) (y : Nat) : Vec A (x + y) := ...
-- @
--
-- as definining a term @A : Type, x : Nat, y : Nat ⊢ foo A x y : Vec (x + y)@.
class SimpleLhs cell lhs => ArgumentLhs cell lhs | lhs -> cell where
  lhsCells :: [cell] -> cell -> lhs

-- | Shorthand for making a synthesized let-binding.
(.=)
  :: (LetDef lhs rep defn, SimpleLhs lc lhs, Syn nm lc)
  => nm -> rep -> defn
nm .= rep = letDef (lhsCell (syn nm)) rep

--------------------------------------------------------------------------------
-- Terms

class (Name nm) => Var nm rep | rep -> nm where
  var :: nm -> rep

varN :: (Var nm rep) => nm -> Natural -> rep
varN nm i = var (nameN nm i)

-- | Pi-types.
class Pi binder rep | rep -> binder, binder -> rep where
  -- | Create a pi type over a @binder@.
  --
  -- See $binders for expected use.
  pi :: binder -> rep -> rep

-- | Let-bindings.
class Let defn rep | rep -> defn, defn -> rep where
  let_ :: defn -> rep -> rep


--------------------------------------------------------------------------------
-- Operators and Builtins

-- | Builtins
--
-- We want to be able to be polymorphic over the builtin-constructs of a language,
-- *and* be able to separately dispatch on builtins when defining a language instance.
-- For example, one language might call the @List@ built-in @List A@, another @[A]@, and so on.
-- Moreover, we don't want to have to define a new class per built-in: this gets really cumbersome
-- really fast, and means that users might end up having to roll their own built-in classes, which
-- is not very good.
--
-- In light of this, we define a single 'Builtin' class that lets us dispatch on a statically known
-- 'Symbol'. We also allow the return type to vary, which lets us implement things like builtin infix operators
-- in a natural way via @Builtin rep "+" (rep -> rep -> rep)@.
class (KnownSymbol op) => Builtin (rep :: Type) (op :: Symbol) (tp :: Type) | rep op -> tp, op tp -> rep where
  -- | Make a builtin of a given type.
  --
  -- When writing generators, users are encouraged to use 'builtin' instead, as it
  -- has *much* better inference.
  mkBuiltin :: tp

-- | Construct a builtin term.
builtin :: forall rep tp. forall op -> (Builtin rep op tp) => tp
builtin o = mkBuiltin @rep @o @tp

type Constant rep op = Builtin rep op rep

-- | Shorthand for a builtin that doesn't have any arguments.
constant :: forall op -> Constant rep op => rep
constant = builtin

type Op1 rep op = Builtin rep op (rep -> rep)

-- | Shorthand for a unary operator.
op1 :: forall op -> Op1 rep op => rep -> rep
op1 = builtin

type Op2 rep op = Builtin rep op (rep -> rep -> rep)

-- | Shorthand for a binary operator.
op2 :: forall op -> Op2 rep op => rep -> rep -> rep
op2 = builtin

-- | Literals
--
-- Literals work like 'Builtin', but with a slight twist.
-- Instead of a single @mkBuiltin :: tp@ method, the 'Literal' class
-- has a single @mkLit :: tp -> rep@ method, which reflects that literals
-- must always be built out of *something*. This also leads to marginally better inference.
class (KnownSymbol sym) => Literal (rep :: Type) (sym :: Symbol) (tp :: Type) | rep sym -> tp where
  -- | Make a builtin of a given type.
  --
  -- When writing generators, users are encouraged to use 'lit' instead, as it
  -- has *much* better inference.
  mkLit :: tp -> rep

-- | Construct a literal term.
lit :: forall sym -> Literal rep sym tp => tp -> rep
lit sym x = mkLit @_ @sym x

-- | Construct a @Nat@ literal.
nat :: (Literal rep "Nat" Natural) => Natural -> rep
nat = lit "Nat"

-- | Construct a @List@ literal.
list :: (Literal rep "List" [rep]) => [rep] -> rep
list = lit "List"

-- | Construct a @Vec@ literal.
vec :: (Literal rep "Vec" [rep]) => [rep] -> rep
vec = lit "Vec"

-- | Construct a @String@ literal.
string :: (Literal rep "String" Text) => Text -> rep
string = lit "String"

--------------------------------------------------------------------------------
-- Top-level modules

class (Monoid m) => Module m defn | defn -> m, m -> defn where
  -- | Construct a top-level module.
  module_
    :: Text -- ^ The name of the module
    -> m    -- ^ Module header.
    -> m    -- ^ Module body.
    -> m

  -- | Create a top-level definition.
  def :: defn -> m

modDefs :: (Module m defn) => [defn] -> m
modDefs = foldMap def

class AnnTermDefn lhs tm defn | defn -> lhs, defn -> tm where
  annTerm :: lhs -> tm -> tm -> defn

--------------------------------------------------------------------------------
-- Imports

class (KnownSymbol i) => Import (m :: Type) (i :: Symbol) where
  mkImport :: m

import_ :: forall i -> (Import m i) => m
import_ i = mkImport @_ @i
