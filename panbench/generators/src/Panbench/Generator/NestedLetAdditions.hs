module Panbench.Generator.NestedLetAdditions where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule Natural hdr defns
generator =
    GenModule "LetAddExample"
      [ import_ "Data.Nat"
      ] \size ->
      [ ([] |- ("n" .: builtin "Nat")) .=
          let_ [ [] |- syn (nameN "x" 0) .= (nat 1)] $
          let_ [ [] |- syn (nameN "x" i) .= (op2 "+" (nameN "x" (i - 1)) (nameN "x" (i - 1))) | i <- [1..size]] $
          nameN "x" size
      ]
