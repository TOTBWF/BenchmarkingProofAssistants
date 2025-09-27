{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.NestedLetFunctions where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Module mod hdr defns
     , Import hdr "Data.Nat"
     , Definition defns topLhs tm, ArgumentLhs topLhsHd topLhsArg topLhs, Chk name tm topLhsHd
     , Name name
     , Constant tm "Nat"
     , Constant tm "Nat", Literal tm "Nat" Natural, Op2 tm "+"
     , Definition letDefn letLhs tm, ArgumentLhs letLhsHd letLhsArg letLhs, Syn nm letLhsHd, Chk name tm letLhsArg
     , Var nm tm, Let letDefn tm, App tm
     )
  => GenModule Natural hdr defns
generator =
    GenModule "LetAddExample"
      [ import_ "Data.Nat"
      ] \size ->
      [ ([] |- ("n" .: builtin "Nat")) .=
          let_ [
            [ nameN "x" j .: builtin "Nat" | j <- [1..i] ] |- syn (nameN "f" i) .=
                foldl (op2 "+") (nat 1) [ varN "x" j | j <- [1..i]]
            | i <- [1..size]
            ] $
          foldr (\i -> op2 "+" (app (varN "f" i) [ nat j | j <- [1..i] ])) (app (varN "f" 1) [nat 2]) [2..size]
      ]
