{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

-- | Pretty printer for Agda.
module Panbench.Grammar.Agda
  ( Agda(..)
  ) where

import Data.Text (Text)

import Data.String (IsString(..))

import Numeric.Natural

import Panbench.Grammar
import Panbench.Pretty

newtype Agda ann = AgdaDoc { getAgda :: Doc ann }
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (Agda ann) where
  nameN = subscript

--------------------------------------------------------------------------------
-- Cells

data AgdaVisibility
  = AgdaVisible
  | AgdaImplicit

data AgdaCell ann
  = AgdaChks AgdaVisibility [(Agda ann)] (Agda ann)
  | AgdaSyns AgdaVisibility [(Agda ann)]

setCellVisibility :: AgdaVisibility -> AgdaCell ann -> AgdaCell ann
setCellVisibility vis (AgdaChks _ nms ann) = AgdaChks vis nms ann
setCellVisibility vis (AgdaSyns _ nms) = AgdaSyns vis nms

instance Syns (Agda ann) (AgdaCell ann) where
  syns = AgdaSyns AgdaVisible

instance Chks (Agda ann) (Agda ann) (AgdaCell ann) where
  chks = AgdaChks AgdaVisible

deriving via SingletonCell (AgdaCell ann) instance Syn (Agda ann) (AgdaCell ann)
deriving via SingletonCell (AgdaCell ann) instance Chk (Agda ann) (Agda ann) (AgdaCell ann)

instance Implicit (AgdaCell ann) where
  implicit = setCellVisibility AgdaImplicit

--------------------------------------------------------------------------------
-- Binders

agdaVisibility :: AgdaVisibility -> Agda ann -> Agda ann
agdaVisibility AgdaVisible = enclose "(" ")"
agdaVisibility AgdaImplicit = enclose "{" "}"

agdaBinder :: AgdaCell ann -> Agda ann
agdaBinder (AgdaChks vis nm ann) = agdaVisibility vis (hsep nm <+> ":" <+> ann)
agdaBinder (AgdaSyns vis nm) = agdaVisibility vis (hsep nm)

agdaBinders :: [AgdaCell ann] -> Agda ann
agdaBinders = hsepMap agdaBinder

--------------------------------------------------------------------------------
-- Terms

instance Var (Agda ann) (Agda ann) where
  var = id

instance Pi [AgdaCell ann] (Agda ann) where
  pi [] body = body
  pi args body = agdaBinders args <\?> "→" <+> body
