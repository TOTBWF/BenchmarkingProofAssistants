{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DataKinds #-}

-- | Pretty printer for Rocq.
module Panbench.Grammar.Rocq
  ( Rocq(..)
  ) where

import Data.Text (Text)

import Data.String (IsString(..))

import Numeric.Natural

import Panbench.Grammar
import Panbench.Pretty

newtype Rocq ann = RocqDoc { getRocq :: Doc ann }
  deriving newtype (Semigroup, Monoid, IsString)

instance Name (Rocq ann) where
  nameN x i = x <> pretty i


--------------------------------------------------------------------------------
-- Cells

data RocqVisibility
  = RocqVisible
  -- ^ A visible argument.
  | RocqMaximalImplicit
  -- ^ A maximal implicit, which are written as @{x}@.
  --
  -- These always get fully instantiated when a function with implicit
  -- arguments is partially applied.
  -- See https://rocq-prover.org/doc/V9.0.0/refman/language/extensions/implicit-arguments.html
  | RocqNonMaximalImplicit
  -- ^ A non-maximal implicit, which are written as @[x]@.
  --
  -- These do not get automatically instantiated when a function with implicit
  -- arguments is partially applied.
  -- See https://rocq-prover.org/doc/V9.0.0/refman/language/extensions/implicit-arguments.html

data RocqCell ann
  = RocqChks RocqVisibility [(Rocq ann)] (Rocq ann)
  | RocqSyns RocqVisibility [(Rocq ann)]

setCellVisibility :: RocqVisibility -> RocqCell ann -> RocqCell ann
setCellVisibility vis (RocqChks _ nms ann) = RocqChks vis nms ann
setCellVisibility vis (RocqSyns _ nms) = RocqSyns vis nms

instance Syns (Rocq ann) (RocqCell ann) where
  syns = RocqSyns RocqVisible

instance Chks (Rocq ann) (Rocq ann) (RocqCell ann) where
  chks = RocqChks RocqVisible

deriving via SingletonCell (RocqCell ann) instance Syn (Rocq ann) (RocqCell ann)
deriving via SingletonCell (RocqCell ann) instance Chk (Rocq ann) (Rocq ann) (RocqCell ann)

-- | To keep feature parity with other languages (Lean, Agda), we opt to treat 'implict' as a maximal
-- implicit.
instance Implicit (RocqCell ann) where
  implicit = setCellVisibility RocqMaximalImplicit

--------------------------------------------------------------------------------
-- Binders

rocqVisibility :: RocqVisibility -> Rocq ann -> Rocq ann
rocqVisibility RocqVisible = enclose "(" ")"
rocqVisibility RocqMaximalImplicit = enclose "{" "}"
rocqVisibility RocqNonMaximalImplicit = enclose "[" "]"

rocqBinder :: RocqCell ann -> Rocq ann
rocqBinder (RocqChks vis nms ann) = rocqVisibility vis (hsep nms <+> ":" <+> ann)
rocqBinder (RocqSyns vis nms) = rocqVisibility vis (hsep nms)

-- | Rocq supports multi-cell pi-types like
--
-- @
-- forall (x y : nat) (p q : x = y), ...
-- @
rocqBinders :: [RocqCell ann] -> Rocq ann
rocqBinders = hsepMap rocqBinder

--------------------------------------------------------------------------------
-- Terms

instance Var (Rocq ann) (Rocq ann) where
  var = id

instance Pi [RocqCell ann] (Rocq ann) where
  pi [] body = body
  pi args body = "forall" <+> rocqBinders args <> "," <\?> body


-- --------------------------------------------------------------------------------
-- -- Patterns

-- --------------------------------------------------------------------------------
-- -- Definitions

-- -- | Print a let-bound local definition.
-- letDef :: Defn Name (Maybe (Rocq ann)) (Rocq ann) -> Rocq ann
-- letDef (Ann nm (Just tp) := tm) =
--   pretty nm <+> ":" <+> tp <+> ":=" <+> tm
-- letDef (Ann nm Nothing := tm) =
--   pretty nm <+> ":=" <+> tm

-- --------------------------------------------------------------------------------
-- -- Terms

-- -- | Non-dependent pi-types.
-- instance Pi (Rocq ann) (Rocq ann) where
--   pi x y = x <+> "->" <+> y


-- -- [implicit x, y] :-> __

-- -- instance Pi (Rocq ann) (Rocq ann) where
-- -- instance Term (Rocq ann) where
-- --   var x = pretty x
-- --   arr x y = x <+> "->" <+> y
-- --   pi (xs :- y) = binders xs <+> "->" <+> y
-- --   app x ys = x <\?> sep ys
-- --   let_ [def] body = "let" <+> letDef def <+> "in" <+> body
-- --   let_ defs body = hardlinesMap (\def -> "let" <+> letDef def <+> "in") defs <\> body
-- --   univ = "Type"
-- --   parens = enclose "(" ")"

-- --------------------------------------------------------------------------------
-- -- Builtins

-- instance Builtin (Rocq ann) "+" (Rocq ann -> Rocq ann -> Rocq ann) where
--   mkBuiltin x y = x <+> "+" <+> y

-- instance Builtin (Rocq ann) "suc" (Rocq ann -> Rocq ann) where
--   mkBuiltin x = "S" <+> x

-- instance Builtin (Rocq ann) "Nat" (Rocq ann) where
--   mkBuiltin = "nat"

-- instance Literal (Rocq ann) "Nat" Natural where
--   mkLit = pretty

-- --------------------------------------------------------------------------------
-- -- Modules

-- instance Module (Rocq ann) (Rocq ann) where
--   module_ modNm header body =
--     hardlines
--     [ header
--     , "Module" <+> pretty modNm <> "."
--     , body
--     , "End" <+> pretty modNm <> "."
--     ]

--   defTm nm tp tm =
--     "Definition" <+> pretty nm <+> ":" <+> tp <+> ":=" <\?> nest 4 tm <> "." <> hardline

--   defMatch nm tp clauses =
--     "Fixpoint" <+> pretty nm <+> ":=" <\> __

--   defData nm params ctors =
--     __

--------------------------------------------------------------------------------
-- Imports

requireImport :: Text -> Rocq ann
requireImport m = "Require" <+> "Import" <+> pretty m <> "." <> hardline

justImport :: Text -> Rocq ann
justImport m = "Import" <+> pretty m <> "." <> hardline

-- | The equivalent of @Data.Nat@ is built-in for Rocq.
instance Import (Rocq ann) "Data.Nat" where
  mkImport = mempty

instance Import (Rocq ann) "Data.Vec" where
  mkImport =
    mconcat
    [ requireImport "Coq.Vectors.Vector"
    , justImport "VectorNotations"
    ]

-- | The equivalent of @Data.List@ is built-in for Rocq.
instance Import (Rocq ann) "Data.List" where
  mkImport = mempty
