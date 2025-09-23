{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

-- | Pretty printer for Lean 4.
module Panbench.Grammar.Lean
  (
  ) where

import Data.String (IsString(..))

import Panbench.Grammar
import Panbench.Pretty

newtype Lean ann = LeanDoc { getLean :: Doc ann }
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (Lean ann) where
  nameN x = subscript x

--------------------------------------------------------------------------------
-- Cells

data LeanVisibility
  = LeanVisible
  | LeanImplicit

data LeanCell ann
  = LeanChks LeanVisibility [(Lean ann)] (Lean ann)
  | LeanSyns LeanVisibility [(Lean ann)]

setCellVisibility :: LeanVisibility -> LeanCell ann -> LeanCell ann
setCellVisibility vis (LeanChks _ nms ann) = LeanChks vis nms ann
setCellVisibility vis (LeanSyns _ nms) = LeanSyns vis nms

instance Syns (Lean ann) (LeanCell ann) where
  syns = LeanSyns LeanVisible

instance Chks (Lean ann) (Lean ann) (LeanCell ann) where
  chks = LeanChks LeanVisible

deriving via SingletonCell (LeanCell ann) instance Syn (Lean ann) (LeanCell ann)
deriving via SingletonCell (LeanCell ann) instance Chk (Lean ann) (Lean ann) (LeanCell ann)

instance Implicit (LeanCell ann) where
  implicit = setCellVisibility LeanImplicit

--------------------------------------------------------------------------------
-- Binders

leanVisibility :: LeanVisibility -> Lean ann -> Lean ann
leanVisibility LeanVisible = enclose "{" "}"
leanVisibility LeanImplicit = enclose "(" ")"

leanBinder :: LeanCell ann -> Lean ann
leanBinder (LeanChks vis nm ann) = leanVisibility vis (hsep nm <+> ":" <+> ann)
leanBinder (LeanSyns vis nm) = leanVisibility vis (hsep nm)

--------------------------------------------------------------------------------
-- Terms

instance Var (Lean ann) (Lean ann) where
  var = id

instance Pi (LeanCell ann) (Lean ann) where
  pi arg body = leanBinder arg <\?> "→" <+> body
