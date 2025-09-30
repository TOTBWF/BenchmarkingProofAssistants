{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Pretty printer for Rocq.
module Panbench.Grammar.Rocq
  ( Rocq
  ) where

import Prelude hiding (pi)

import Data.Coerce
import Data.Functor
import Data.Functor.Identity
import Data.String (IsString(..))
import Data.Text (Text)

import Numeric.Natural

import Panbench.Grammar.Cell
import Panbench.Grammar
import Panbench.Pretty

-- | Type-level symbol for Rocq.
data Rocq ann

--------------------------------------------------------------------------------
-- Names

newtype RocqName ann = RocqnName (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (RocqName ann) where
  nameN x i = x <> pretty i

--------------------------------------------------------------------------------
-- Cells

data RocqVis
  = Visible
  | MaximalImplicit
  -- ^ A maximal implicit, which are written as @{x}@.
  --
  -- These always get fully instantiated when a function with implicit
  -- arguments is partially applied.
  -- See https://rocq-prover.org/doc/V9.0.0/refman/language/extensions/implicit-arguments.html
  | NonMaximalImplicit
  -- ^ A non-maximal implicit, which are written as @[x]@.
  --
  -- These do not get automatically instantiated when a function with implicit
  -- arguments is partially applied.
  -- See https://rocq-prover.org/doc/V9.0.0/refman/language/extensions/implicit-arguments.html
  deriving (Eq)

type RocqMultiCell info ann = MultiCell info (RocqName ann) (RocqTm ann)
type RocqSingleCell info ann = SingleCell info (RocqName ann) (RocqTm ann)
type RocqRequiredCell info ann = Cell info Identity (RocqName ann) Identity (RocqTm ann)

type RocqTelescope hdInfo hdAnn docAnn = CellTelescope
   RocqVis [] (RocqName docAnn) Maybe (RocqTm docAnn)
   hdInfo Identity (RocqName docAnn) hdAnn (RocqTm docAnn)

-- | Apply a Rocq visibility modifier to a document.
rocqVis :: (IsDoc doc, IsString (doc ann)) => RocqVis -> doc ann -> doc ann
rocqVis Visible = enclose "(" ")"
rocqVis MaximalImplicit = enclose "{" "}"
rocqVis NonMaximalImplicit = enclose "[" "]"

-- | Render a Rocq binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
rocqCell
  :: (Foldable arity, Foldable tpAnn , IsDoc doc, IsString (doc ann))
  => Cell RocqVis arity (RocqName ann) tpAnn (RocqTm ann)
  -> doc ann
rocqCell (Cell vis names tp)
  | null tp = rocqVis vis (hsepMap coerce names)
  | otherwise = rocqVis vis (hsepMap coerce names <+> ":" <+> hsepMap coerce tp)

rocqCells
  :: (Foldable arity, Foldable tpAnn, IsDoc doc, IsString (doc ann))
  => [Cell RocqVis arity (RocqName ann) tpAnn (RocqTm ann)]
  -> doc ann
rocqCells = hsepMap rocqCell

--------------------------------------------------------------------------------
-- Top-level definitions

newtype RocqDefn ann = RocqDefn (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

type RocqTmDefnLhs ann = RocqTelescope () Maybe ann

instance Definition (RocqDefn ann) (RocqTmDefnLhs ann) (RocqTm ann) where
  (tele :- SingleCell _ nm tp) .= tm =
    doc $
    nest 4 $
    "Definition" <+> undoc nm <+> rocqCells tele <> undoc (maybe mempty (":" <+>) tp) <+> ":=" <\?>
      undoc tm <> "."

type RocqPostulateDefnLhs ann = RocqTelescope () Identity ann

instance Postulate (RocqDefn ann) (RocqPostulateDefnLhs ann) where
  postulate (tele :- RequiredCell _ nm tp) =
    doc $
    nest 4 $
    "Axiom" <+> undoc nm <+> ":" <\?>
      undoc (pi tele tp) <> "."

type RocqDataDefnLhs ann = RocqTelescope () Identity ann

instance DataDefinition (RocqDefn ann) (RocqDataDefnLhs ann) (RocqRequiredCell () ann) where
  data_ (params :- RequiredCell _ nm tp) ctors =
    doc $
    "Inductive" <+> undoc nm <+> rocqCells params <+> ":" <+> undoc tp <\>
    hardlinesFor ctors (\(RequiredCell _ nm tp) -> nest 4 ("|" <+> undoc nm <+> ":" <\?> undoc tp)) <> "."

-- [TODO: Reed M, 29/09/2025] Technically rocq can omit type signatures on records.
type RocqRecordDefnLhs ann = RocqTelescope () Identity ann

instance RecordDefinition (RocqDefn ann) (RocqRecordDefnLhs ann) (RocqName ann) (RocqRequiredCell () ann) where
  record_ (params :- (RequiredCell _ nm tp)) ctor fields =
    doc $
    nest 2 $
    "Record" <+> undoc nm <+> rocqCells params <+> ":" <+> undoc tp <+> ":=" <+>
      group (undoc ctor <\?> "{ " <+> nest 2 (vsep (punctuate ";" (fields <&> \(RequiredCell _ nm tp) -> undoc nm <+> ":" <+> undoc tp))) <+> "}.")

instance Newline (RocqDefn ann) where
  newlines n = hardlines (replicate (fromIntegral n) mempty)

--------------------------------------------------------------------------------
-- Let Bindings
--
-- Right now, these are identical to top-level bindings, but in the future they
-- will include different left-hand sides.

newtype RocqLet ann = RocqLet (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

type RocqLetDefnLhs ann = RocqTelescope () Maybe ann

instance Definition (RocqLet ann) (RocqLetDefnLhs ann) (RocqTm ann) where
  (tele :- (SingleCell _ nm tp)) .= tm =
    doc $
    nest 4 $
    undoc nm <+> rocqCells tele <> undoc (maybe mempty (":" <+>) tp) <+> ":=" <\?> undoc tm

instance Let (RocqLet ann) (RocqTm ann) where
  let_ defns tm =
    doc $ foldr (\defn e -> "let" <+> undoc defn <+> "in" <\?> e) (undoc tm) defns

--------------------------------------------------------------------------------
-- Terms

newtype RocqTm ann = RocqTm (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (RocqTm ann) where
  nameN x i = x <> pretty i

instance Pi (RocqTm ann) (RocqMultiCell RocqVis ann) where
  pi [] body = body
  pi args tp = "forall" <+> rocqCells args <> "," <+> tp

instance Underscore (RocqTm ann) where
  underscore = "_"

instance Literal (RocqTm ann) "Nat" Natural where
  mkLit = pretty

instance Builtin (RocqTm ann) "Nat" (RocqTm ann) where
  mkBuiltin = "nat"

instance Builtin (RocqTm ann) "+" (RocqTm ann -> RocqTm ann -> RocqTm ann) where
  mkBuiltin x y = x <+> "+" <+> y

--------------------------------------------------------------------------------
-- Modules

newtype RocqHeader ann = RocqHeader (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

newtype RocqMod ann = RocqMod { getRocqMod :: Doc ann }
  deriving newtype (Semigroup, Monoid, IsString)

instance Module (RocqMod ann) (RocqHeader ann) (RocqDefn ann) where
  module_ nm header body =
    doc $ hardlines
    [ undoc header
    , mempty
    , "Module" <+> pretty nm <+> "."
    , undoc body
    , "End" <+> pretty nm <+> "."
    ]

--------------------------------------------------------------------------------
-- Imports

requireImport :: Text -> RocqHeader ann
requireImport m = "Require" <+> "Import" <+> pretty m <> "."

justImport :: Text -> RocqHeader ann
justImport m = "Import" <+> pretty m <> "."

-- | The equivalent of @Data.Nat@ is built-in for Rocq.
instance Import (RocqHeader ann) "Data.Nat" where
  mkImport = mempty

-- | The equivalent of @Data.List@ is built-in for Rocq.
instance Import (RocqHeader ann) "Data.List" where
  mkImport = mempty
