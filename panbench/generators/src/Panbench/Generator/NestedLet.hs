{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.NestedLet where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Module mod hdr defns
     , Import hdr "Data.Nat"
     , Definition defns topLhs tm, ArgumentLhs topLhsHd topLhsArg topLhs, Chk name tm topLhsHd
     , Name name
     , Constant tm "Nat"
     , Constant tm "Nat", Literal tm "Nat" Natural
     , Definition letDefn letLhs tm, ArgumentLhs letLhsHd letLhsArg letLhs, Syn nm letLhsHd
     , Var nm tm, Let letDefn tm
     )
  => GenModule Natural hdr defns
generator =
    GenModule "LetExample"
      [ import_ "Data.Nat"
      ] \size ->
      [ ([] |- (nameN "n" 0 .: builtin "Nat")) .=
          let_ [ [] |- syn (nameN "x" 0) .= (nat 1)] $
          let_ [ [] |- syn (nameN "x" i) .= varN "x" (i - 1) | i <- [1..size]] $
          varN "x" size
      ]
