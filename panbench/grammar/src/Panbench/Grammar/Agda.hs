{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}

-- | Pretty printer for Agda.
module Panbench.Grammar.Agda
  ( Agda(..)
  ) where

import Prelude hiding (pi)

import Data.Text (Text)

import Data.Maybe
import Data.String (IsString(..))

import Panbench.Grammar
import Panbench.Pretty

newtype Agda ann = AgdaDoc { getAgda :: Doc ann }
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (Agda ann) where
  nameN = subscript

--------------------------------------------------------------------------------
-- Cells

data Visibility
  = Visible
  -- ^ Visible arguments like @(x : A)@.
  | Implicit
  -- ^ Implicit arguments like @(x : A)@.
  deriving (Eq)

data Cell ann =
  Cell
  { cellVis :: Visibility
  -- ^ Visibility for cells.
  , cellNames :: [Agda ann]
  -- ^ Names being bound by this cell.
  , cellAnn :: Maybe (Agda ann)
  -- ^ An optional type annotation.
  }

instance Syns (Agda ann) (Cell ann) where
  syns nms = Cell Visible nms Nothing

instance Chks (Agda ann) (Agda ann) (Cell ann) where
  chks nms ann = Cell Visible nms (Just ann)

deriving via SingletonCell (Cell ann) instance Syn (Agda ann) (Cell ann)
deriving via SingletonCell (Cell ann) instance Chk (Agda ann) (Agda ann) (Cell ann)

instance Implicit (Cell ann) where
  implicit cell = cell { cellVis = Implicit }

-- | View a 'Cell' that has an annotation.
viewChkCell :: Cell ann -> Maybe (Visibility, [(Agda ann)], Agda ann)
viewChkCell (Cell _ _ Nothing) = Nothing
viewChkCell (Cell vis nms (Just ann)) = Just (vis, nms, ann)

-- | View a 'Cell' that lacks an annotation.
viewSynCell :: Cell ann -> Maybe (Visibility, [(Agda ann)])
viewSynCell (Cell _ _ (Just _)) = Nothing
viewSynCell (Cell vis nms Nothing) = Just (vis, nms)

--------------------------------------------------------------------------------
-- Binders

-- | Surround a document with the appropriate delimiters for a given 'Visibility'.
withVisibility :: Visibility -> Agda ann -> Agda ann
withVisibility Visible = enclose "(" ")"
withVisibility Implicit = enclose "{" "}"

-- | Render a 'Cell'.
cell :: Cell ann -> Agda ann
cell (Cell vis nm (Just ann)) = withVisibility vis (hsep nm <+> ":" <+> ann)
cell (Cell vis nm Nothing) = withVisibility vis (hsep nm)

-- | Render a list of 'Cell's.
cells :: [Cell ann] -> Agda ann
cells = hsepMap cell

--------------------------------------------------------------------------------
-- Let-binding left-hand-sides

-- | A Left-hand side head for a local let binding.
data LetLhsHead ann =
  LetLhsHead
  { letLhsHeadName :: Agda ann
  -- ^ The name of the left-hand side head.
  , letLhsHeadAnn :: Maybe (Agda ann)
  -- ^ The return type of the let binding.
  --
  -- EG: for a binding like
  --
  -- @
  -- let replicate : {ℓ} {A : Set ℓ} (x : Nat) → A → Vec A n
  -- let replicate = ...
  -- @
  --
  -- The corresponding @letLhsHeadAnn@ is @Just (Vec A n)@.
  }

-- | A left-hand side for a local let binding.
data LetLhs ann =
  LetLhs
  { letLhsHead :: LetLhsHead ann
  -- ^ The head of the let binding.
  , letLhsArgs :: [[Cell ann]]
  -- ^ Telescope of arguments to the let binding.
  --
  -- We use a list of lists here to allow us to retain grouping information
  -- for type annotations.
  }

-- | Compute a type annotation for a local let binding.
--
-- If the head and all arguments are unannotated, then we consider
-- the entire let binding to be un-annotated.
--
-- If we have annotated arguments but the head is unannoted, then we generate
-- a pi type with an underscore for the return type. like @(x : A) -> _@.
letLhsTpAnn :: LetLhs ann -> Maybe (Agda ann)
letLhsTpAnn (LetLhs (LetLhsHead nm Nothing) (traverse (traverse viewSynCell) -> Nothing)) = Nothing
letLhsTpAnn (LetLhs (LetLhsHead nm ann) cells) =
  let returnTp = fromMaybe "_" ann
      tp = foldr (\cells tp -> pi cells tp) returnTp cells
  in Just tp

instance SimpleLhs (LetLhsHead ann) (LetLhs ann) where
  lhsHd hd = LetLhs hd []

instance ArgumentLhs (LetLhsHead ann) [(Cell ann)] (LetLhs ann) where
  lhsArgs cells hd = LetLhs hd cells

--------------------------------------------------------------------------------
-- Let bindings

-- | A single let binding, sans the body.
data LetBinding ann =
  LetBinding
  { letBindingLhs :: LetLhs ann
  -- ^ The left-hand side of a let binding.
  , letBindingRhs :: Agda ann
  -- ^ The right-hand side of a let binding
  }

instance LetDef (LetLhs ann) (Agda ann) [LetBinding ann] where
  letDef lhs e = [LetBinding lhs e]

instance LetDefs [LetBinding ann] where
  letDefs = concat

-- | Render a single let binding.
--
-- [FIXME: Reed M, 25/09/2025] We discard any visibility information when rendering out
-- argument binders here.
letBindingTpAnn :: LetBinding ann -> (Maybe (Agda ann), Agda ann)
letBindingTpAnn (LetBinding lhs@(LetLhs (LetLhsHead nm _) args) rhs) =
  ( fmap (nm <+> ":" <+>) (letLhsTpAnn lhs)
  , nm <+> hsep (getVisibleArgs args) <+> "=" <\?> rhs
  )
  where
    -- Get only the visible arguments and remove any cell grouping.
    getVisibleArgs :: [[Cell ann]] -> [Agda ann]
    getVisibleArgs = concatMap $ concatMap \cell ->
      case cellVis cell of
        Visible -> cellNames cell
        _ -> []

-- | Render a let binding sans the @let@ and @in e@.
letBinding :: LetBinding ann -> Agda ann
letBinding def =
  case letBindingTpAnn def of
    (Just ann, def) -> ann <\> def
    (Nothing, def) -> def

--------------------------------------------------------------------------------
-- Terms

instance Var (Agda ann) (Agda ann) where
  var = id

instance Pi [Cell ann] (Agda ann) where
  pi [] body = body
  pi args body = cells args <\?> "→" <+> body

instance Let [LetBinding ann] (Agda ann) where
  let_ [] body =
    body
  let_ [letBindingTpAnn -> (Nothing, def)] body =
    -- For single un-annotated let bindings, we do our best to try and lay things
    -- out on a single line.
    "let" <+> nest 4 def <\?> "in" <+> body
  let_ defs body =
    "let" <+> nest 4 (hardlinesMap letBinding defs) <\> "in" <+> body

-- --------------------------------------------------------------------------------
-- -- Modules

-- -- instance Module (Agda ann) (Agda ann) where
-- --   module_ nm header body =
-- --     hardlines
-- --     [ "module" <+> pretty nm <+> "where"
-- --     , mempty
-- --     , header
-- --     , mempty
-- --     , body
-- --     ]

-- --   def = id

-- -- --------------------------------------------------------------------------------
-- -- -- Top-level definitions

-- data AgdaTopLhsHd ann
--   = AgdaTopLhsHd
--   { agdaTopLhsName :: Agda ann
--   -- ^ The name of the top-level binding.
--   , agdaTopLhsAnn  :: Maybe (Agda ann)
--   -- ^ The return type of the top level binding.
--   --
--   -- EG: for a binding like
--   --
--   -- @
--   -- replicate : {ℓ} {A : Set ℓ} (x : Nat) → A → Vec A n
--   -- replicate = ...
--   -- @
--   --
--   -- The corresponding @agdaTopLhsAnn@ is @Vec A n@.
--   }

-- data AgdaTopLhs ann
--   = AgdaTopLhs
--   { agdaTopLhsHd   :: AgdaTopLhsHd ann
--   -- ^ The head of the top level definition.
--   , agdaTopLhsArgs :: [(Cell ann)]
--   -- ^ Arguments to the top level definition.
--   }

-- instance SimpleLhs (AgdaTopLhsHd ann) (AgdaTopLhs ann) where
--   lhsHd hd = AgdaTopLhs hd []

-- instance ArgumentLhs (AgdaTopLhsHd ann) [Cell ann] (AgdaTopLhs ann) where
--   lhsArgs args hd = __

--------------------------------------------------------------------------------
-- Imports

openImport :: Text -> Agda ann
openImport m = "open" <+> "import" <+> pretty m <> hardline

instance Import (Agda ann) "Data.Nat" where
  mkImport = openImport "Agda.Builtin.Nat"

instance Import (Agda ann) "Data.List" where
  mkImport = openImport "Agda.Builtin.List"

instance Import (Agda ann) "Data.String" where
  mkImport = openImport "Agda.Builtin.String"
