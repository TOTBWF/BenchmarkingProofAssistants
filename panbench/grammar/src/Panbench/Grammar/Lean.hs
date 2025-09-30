{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Pretty printer for Lean 4.
module Panbench.Grammar.Lean
  ( Lean
  ) where

import Data.Coerce
import Data.String (IsString(..))
import Data.Functor.Identity

import Numeric.Natural

import Panbench.Grammar.Cell
import Panbench.Grammar
import Panbench.Pretty

-- | Type-level symbol for Lean.
data Lean ann

--------------------------------------------------------------------------------
-- Names

newtype LeanName ann = LeanName (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (LeanName ann) where
  nameN x = subscript x

--------------------------------------------------------------------------------
-- Cells

data LeanVis
  = Visible
  -- ^ Visible arguments.
  | Implicit
  -- ^ Implicit arguments, which are written as @{x}@.
  | SemiImplicit
  -- ^ Semi-implicit arguments, which are written as @{{x}}@.
  deriving (Eq)

type LeanMultiCell info ann = MultiCell info (LeanName ann) (LeanTm ann)
type LeanSingleCell info ann = SingleCell info (LeanName ann) (LeanTm ann)
type LeanRequiredCell info ann = Cell info Identity (LeanName ann) Identity (LeanTm ann)

type LeanTelescope hdInfo hdAnn docAnn = CellTelescope
   LeanVis [] (LeanName docAnn) Maybe (LeanTm docAnn)
   hdInfo Identity (LeanName docAnn) hdAnn (LeanTm docAnn)

-- | Apply a lean 4 visibility modifier to a document.
leanVis :: (IsDoc doc, IsString (doc ann)) => LeanVis -> doc ann -> doc ann
leanVis Visible = enclose "(" ")"
leanVis Implicit = enclose "{" "}"
leanVis SemiImplicit = enclose "{{" "}}"

-- | Render a Lean binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
leanCell
  :: (Foldable arity, Foldable tpAnn , IsDoc doc, IsString (doc ann))
  => Cell LeanVis arity (LeanName ann) tpAnn (LeanTm ann)
  -> doc ann
leanCell (Cell vis names tp)
  | null tp = leanVis vis (hsepMap coerce names)
  | otherwise = leanVis vis (hsepMap coerce names <+> ":" <+> hsepMap coerce tp)

leanCells
  :: (Foldable arity, Foldable tpAnn, IsDoc doc, IsString (doc ann))
  => [Cell LeanVis arity (LeanName ann) tpAnn (LeanTm ann)]
  -> doc ann
leanCells = hsepMap leanCell

--------------------------------------------------------------------------------
-- Top-level definitions

newtype LeanDefn ann = LeanDefn (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

type LeanTmDefnLhs ann = LeanTelescope () Maybe ann

instance Definition (LeanDefn ann) (LeanTmDefnLhs ann) (LeanTm ann) where
  (tele :- SingleCell _ nm tp) .= tm =
    doc $
    nest 4 $
    "def" <+> undoc nm <+> leanCells tele <> undoc (maybe mempty (":" <+>) tp) <+> ":=" <\?>
      undoc tm

type LeanPostulateDefnLhs ann = LeanTelescope () Identity ann

instance Postulate (LeanDefn ann) (LeanPostulateDefnLhs ann) where
  postulate (tele :- RequiredCell _ nm tp) =
    doc $
    nest 4 $
    "axiom" <+> undoc nm <+> leanCells tele <+> ":" <+> undoc tp

type LeanDataDefnLhs ann = LeanTelescope () Identity ann

instance DataDefinition (LeanDefn ann) (LeanDataDefnLhs ann) (LeanRequiredCell () ann) where
  data_ (params :- RequiredCell _ nm tp) ctors =
    doc $
    nest 2 $
    "inductive" <+> undoc nm <+> leanCells params <+> ":" <+> undoc tp <+> "where" <\>
      hardlinesFor ctors \(RequiredCell _ ctorNm ctorTp) ->
        "|" <+> undoc ctorNm <+> ":" <+> undoc ctorTp

type LeanRecordDefnLhs ann = LeanTelescope () Identity ann

instance RecordDefinition (LeanDefn ann) (LeanRecordDefnLhs ann) (LeanName ann) (LeanRequiredCell () ann) where
  record_ (params :- RequiredCell _ nm tp) ctor fields =
    doc $
    nest 2 $
    "structure" <+> undoc nm <+> leanCells params <+> ":" <+> undoc tp <\>
      undoc ctor <+> "::" <\>
      hardlinesFor fields \(RequiredCell _ fieldNm fieldTp) ->
       undoc fieldNm <+> ":" <+> undoc fieldTp

instance Newline (LeanDefn ann) where
  newlines n = hardlines (replicate (fromIntegral n) mempty)

--------------------------------------------------------------------------------
-- Let Bindings

newtype LeanLet ann = LeanLet (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

type LeanLetDefnLhs ann = LeanTelescope () Maybe ann

instance Definition (LeanLet ann) (LeanLetDefnLhs ann) (LeanTm ann) where
  (tele :- SingleCell _ nm tp) .= tm =
    doc $
    nest 4 $
    undoc nm <+> leanCells tele <> undoc (maybe mempty (":" <+>) tp) <+> ":=" <\?> undoc tm

instance Let (LeanLet ann) (LeanTm ann) where
  let_ defns tm =
    doc $
    undoc (hardlinesMap ("let" <+>) defns) <\>
    undoc tm

--------------------------------------------------------------------------------
-- Terms

newtype LeanTm ann = LeanTm (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (LeanTm ann) where
  nameN = subscript

instance Pi (LeanTm ann) (LeanMultiCell LeanVis ann) where
  pi arg body = leanCells arg <\?> "→" <+> body

instance Underscore (LeanTm ann) where
  underscore = "_"

instance Literal (LeanTm ann) "Nat" Natural where
  mkLit = pretty

instance Builtin (LeanTm ann) "Nat" (LeanTm ann) where
  mkBuiltin = "Nat"

instance Builtin (LeanTm ann) "+" (LeanTm ann -> LeanTm ann -> LeanTm ann) where
  mkBuiltin x y = x <+> "+" <+> y

--------------------------------------------------------------------------------
-- Modules

newtype LeanHeader ann = LeanHeader (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

newtype LeanMod ann = LeanMod { getLeanMod :: Doc ann }
  deriving newtype (Semigroup, Monoid, IsString)

instance Module (LeanMod ann) (LeanHeader ann) (LeanDefn ann) where
  module_ _ header body =
    doc $ hardlines
    [ undoc header
    , undoc body
    ]

--------------------------------------------------------------------------------
-- Imports

-- | The equivalent of @Data.Nat@ is built-in for Lean.
instance Import (LeanHeader ann) "Data.Nat" where
  mkImport = mempty

-- | The equivalent of @Data.List@ is built-in for Lean.
instance Import (LeanHeader ann) "Data.List" where
  mkImport = mempty
