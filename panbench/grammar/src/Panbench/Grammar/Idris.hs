{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

-- | Pretty printer for Idris.
module Panbench.Grammar.Idris
  ( Idris(..)
  ) where

import Data.String (IsString(..))

import Panbench.Grammar
import Panbench.Pretty

newtype Idris ann = IdrisDoc { getIdris :: Doc ann }
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (Idris ann) where
  nameN x i = x <> pretty i

--------------------------------------------------------------------------------
-- Cells

data IdrisVisibility
  = IdrisVisible
  | IdrisImplicit

-- | Idris 2 does not support telescopic binders, but does support
-- multi-binding check cells using the syntax @(m, n : Nat)@.
--
-- Moreover, Idris 2 doesn't have dedicated syntax for unannotated binding
-- cells, and opts to use @{m : _}@ or @{m, n : _}@
data IdrisCell ann
  = IdrisChks IdrisVisibility [(Idris ann)] (Idris ann)
  | IdrisSyns IdrisVisibility [(Idris ann)]

-- | Set the visibility of a binding cell.
setCellVisibility :: IdrisVisibility -> IdrisCell ann -> IdrisCell ann
setCellVisibility vis (IdrisChks _ nms ann) = IdrisChks vis nms ann
setCellVisibility vis (IdrisSyns _ nms) = IdrisSyns vis nms

instance Chks (Idris ann) (Idris ann) (IdrisCell ann) where
  chks = IdrisChks IdrisVisible

instance Syns (Idris ann) (IdrisCell ann) where
  syns = IdrisSyns IdrisVisible

deriving via SingletonCell (IdrisCell ann) instance Syn (Idris ann) (IdrisCell ann)
deriving via SingletonCell (IdrisCell ann) instance Chk (Idris ann) (Idris ann) (IdrisCell ann)

instance Implicit (IdrisCell ann) where
  implicit = setCellVisibility IdrisImplicit

--------------------------------------------------------------------------------
-- Binders

idrisVisibility :: IdrisVisibility -> Idris ann -> Idris ann
idrisVisibility IdrisVisible = enclose "(" ")"
idrisVisibility IdrisImplicit = enclose "{" "}"

idrisBinder :: IdrisCell ann -> Idris ann
idrisBinder (IdrisChks vis nms ann) = idrisVisibility vis (hsep (punctuate "," nms) <+> ":" <+> ann)
idrisBinder (IdrisSyns vis nms) = idrisVisibility vis (hsep (punctuate "," nms) <+> ":" <+> "_")

--------------------------------------------------------------------------------
-- Terms

instance Var (Idris ann) (Idris ann) where
  var = id

instance Pi (IdrisCell ann) (Idris ann) where
  pi arg body = idrisBinder arg <+> "->" <+> body
