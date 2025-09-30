{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Pretty printer for Agda.
module Panbench.Grammar.Agda
  ( Agda
  , AgdaMod(..)
  , AgdaHeader(..)
  , AgdaDefn(..)
  , AgdaTm(..)
  ) where

import Prelude hiding (pi)

import Data.Coerce
import Data.Functor.Identity
import Data.Maybe
import Data.String (IsString(..))
import Data.Text (Text)

import Numeric.Natural

import Panbench.Grammar.Cell
import Panbench.Grammar
import Panbench.Pretty

data Agda ann

newtype AgdaTm ann = AgdaTm (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

newtype AgdaName ann = AgdaName (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (AgdaName ann) where
  nameN = subscript

--------------------------------------------------------------------------------
-- Cells

data AgdaVis
  = Visible
  -- ^ Visible arguments like @(x : A)@.
  | Implicit
  -- ^ Implicit arguments like @(x : A)@.
  deriving (Eq)

type AgdaMultiCell info ann = MultiCell info (AgdaName ann) (AgdaTm ann)
type AgdaSingleCell info ann = SingleCell info (AgdaName ann) (AgdaTm ann)
type AgdaRequiredCell info ann = Cell info Identity (AgdaName ann) Identity (AgdaTm ann)

-- | Surround a document with the appropriate delimiters for a given 'Visibility'.
agdaVis :: (IsDoc doc, IsString (doc ann)) => AgdaVis -> doc ann -> doc ann
agdaVis Visible = enclose "(" ")"
agdaVis Implicit = enclose "{" "}"


-- | Render an Agda binding cell.
--
-- We use a bit of a trick here for annotations. Both 'Identity' and 'Maybe' are 'Foldable', so
-- we can write a single function that handles optional and required annotations by checking if
-- the annotation is empty with 'null', and then folding over it to actually print.
agdaCell :: (Foldable arity, Foldable tpAnn, IsDoc doc, IsString (doc ann)) => Cell AgdaVis arity (AgdaName ann) tpAnn (AgdaTm ann) -> doc ann
agdaCell (Cell vis names tp) | null tp = agdaVis vis (hsepMap coerce names)
                             | otherwise = agdaVis vis (hsepMap coerce names <+> ":" <+> hsepMap coerce tp)

agdaCells :: (Foldable arity, Foldable tpAnn, IsDoc doc, IsString (doc ann)) => [Cell AgdaVis arity (AgdaName ann) tpAnn (AgdaTm ann)] -> doc ann
agdaCells = hsepMap agdaCell

--------------------------------------------------------------------------------
-- Top-level definitions

newtype AgdaDefn ann = AgdaDefn (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

-- [TODO: Reed M, 27/09/2025] This feels bad, and should probably be some sort of newtype deriving?
data AgdaTmDefnLhs ann
  = AgdaTmDefnLhs [AgdaMultiCell AgdaVis ann] (AgdaSingleCell () ann)

instance TelescopeLhs (AgdaTmDefnLhs ann) (AgdaSingleCell () ann) (AgdaMultiCell AgdaVis ann) where
  (|-) = AgdaTmDefnLhs

instance Definition (AgdaDefn ann) (AgdaTmDefnLhs ann) (AgdaTm ann) where
  (AgdaTmDefnLhs (UnAnnotatedCells tele) (UnAnnotatedCell (SingleCell _ nm _))) .= e =
    doc $
    nest 4 (undoc nm <+> agdaCells tele <+> "=" <\?> undoc e)
  (AgdaTmDefnLhs tele (SingleCell _ nm ann)) .= e =
    doc $
    nest 4 (undoc nm <+> ":" <+> undoc (pi tele (fromMaybe underscore ann))) <\>
    nest 4 (undoc nm <+> agdaCells tele <+> "=" <\?> undoc e)

data AgdaPostulateDefnLhs ann
  = AgdaPostulateDefnLhs [AgdaMultiCell AgdaVis ann] (AgdaRequiredCell () ann)

instance Postulate (AgdaDefn ann) (AgdaPostulateDefnLhs ann) where
  postulate (AgdaPostulateDefnLhs tele (SingleCell _ nm (Identity ann))) =
    doc $
    nest 4 (undoc nm <+> ":" <+> undoc (pi tele ann))

data AgdaDataDefnLhs ann
  = AgdaDataDefnLhs [AgdaMultiCell AgdaVis ann] (AgdaRequiredCell () ann)

instance DataDefinition (AgdaDefn ann) (AgdaDataDefnLhs ann) (AgdaRequiredCell () ann) where
  data_ (AgdaDataDefnLhs params (SingleCell _ nm (Identity tp))) ctors =
    doc $
    nest 2 $
      "data" <+> undoc nm <+> agdaCells params <+> ":" <+> undoc tp <+> "where" <\>
      hardlinesFor ctors \(SingleCell _ nm (Identity tp)) ->
        nest 2 $ undoc nm <+> ":" <\?> undoc tp

data AgdaRecordDefnLhs ann
  = AgdaRecordDefnLhs [AgdaMultiCell AgdaVis ann] (AgdaRequiredCell () ann)

instance RecordDefinition (AgdaDefn ann) (AgdaRecordDefnLhs ann) (AgdaName ann) (AgdaRequiredCell () ann) where
  record_ (AgdaRecordDefnLhs params (SingleCell _ nm (Identity tp))) ctor fields =
    doc $
    nest 2 $ hardlines
    [ "record" <+> undoc nm <+> agdaCells params <+> ":" <+> undoc tp <+> "where"
    , "constructor" <+> undoc ctor
    , nest 2 $ hardlines
      [ "fields"
      , hardlinesFor fields \(SingleCell _ nm (Identity tp)) ->
        nest 2 $ undoc nm <+> ":" <\?> undoc tp
      ]
    ]

instance Newline (AgdaDefn ann) where
  newlines n = hardlines (replicate (fromIntegral n) mempty)

--------------------------------------------------------------------------------
-- Let Bindings
--
-- Right now, these are identical to top-level bindings, but in the future they
-- will include different left-hand sides.

newtype AgdaLet ann = AgdaLet (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

data AgdaLetDefnLhs ann
  = AgdaLetDefnLhs [AgdaMultiCell AgdaVis ann] (AgdaSingleCell () ann)

instance TelescopeLhs (AgdaLetDefnLhs ann) (AgdaSingleCell () ann) (AgdaMultiCell AgdaVis ann) where
  (|-) = AgdaLetDefnLhs

instance Definition (AgdaLet ann) (AgdaLetDefnLhs ann) (AgdaTm ann) where
  (AgdaLetDefnLhs (UnAnnotatedCells tele) (UnAnnotatedCell (SingleCell _ nm _))) .= e =
    doc $
    nest 4 (undoc nm <+> undoc (hsepMap (hsep . cellNames) tele) <+> "=" <\?> undoc e)
  (AgdaLetDefnLhs tele (SingleCell _ nm ann)) .= e =
    doc $
    nest 4 (undoc nm <+> ":" <+> undoc (pi tele (fromMaybe underscore ann))) <\>
    nest 4 (undoc nm <+> undoc (hsepMap (hsep . cellNames) tele) <+> "=" <\?> undoc e)

instance Let (AgdaLet ann) (AgdaTm ann) where
  let_ [] e = e
  let_ defns e =
    doc $
    group ("let" <+> undoc (hardlines defns) <\?> "in" <+> undoc e)


--------------------------------------------------------------------------------
-- Terms

instance Name (AgdaTm ann) where
  nameN = subscript

instance Pi (AgdaTm ann) (AgdaMultiCell AgdaVis ann) where
  pi [] body = body
  pi args body = agdaCells args <\?> "→" <+> body

instance Underscore (AgdaTm ann) where
  underscore = "_"

instance Literal (AgdaTm ann) "Nat" Natural where
  mkLit = pretty

instance Builtin (AgdaTm ann) "Nat" (AgdaTm ann) where
  mkBuiltin = "Nat"

instance Builtin (AgdaTm ann) "+" (AgdaTm ann -> AgdaTm ann -> AgdaTm ann) where
  mkBuiltin x y = x <+> "+" <+> y

--------------------------------------------------------------------------------
-- Modules

newtype AgdaMod ann = AgdaMod { getAgda :: Doc ann }
  deriving newtype (Semigroup, Monoid, IsString)

newtype AgdaHeader ann = AgdaHeader (Doc ann)
  deriving newtype (Semigroup, Monoid, IsString)

instance Module (AgdaMod ann) (AgdaHeader ann) (AgdaDefn ann) where
  module_ nm header body =
    doc $ hardlines
    [ "module" <+> pretty nm <+> "where"
    , mempty
    , undoc header
    , mempty
    , undoc body
    ]

--------------------------------------------------------------------------------
-- Imports

openImport :: Text -> AgdaHeader ann
openImport m = "open" <+> "import" <+> pretty m <> hardline

instance Import (AgdaHeader ann) "Data.Nat" where
  mkImport = openImport "Agda.Builtin.Nat"

instance Import (AgdaHeader ann) "Data.List" where
  mkImport = openImport "Agda.Builtin.List"

instance Import (AgdaHeader ann) "Data.String" where
  mkImport = openImport "Agda.Builtin.String"
