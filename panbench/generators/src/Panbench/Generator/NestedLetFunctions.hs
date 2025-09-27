module Panbench.Generator.NestedLetFunctions where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule Natural hdr defns
generator =
    GenModule "LetAddExample"
      [ import_ "Data.Nat"
      ] \size ->
      [ ([] |- ("n" .: builtin "Nat")) .=
          let_ [
            [ nameN "x" j .: builtin "Nat" | j <- [1..i] ] |- syn (nameN "f" i) .=
                foldl (op2 "+") (nat 1) [ nameN "x" j | j <- [1..i]]
            | i <- [1..size]
            ] $
          foldr (\i -> op2 "+" (app (nameN "f" i) [ nat j | j <- [1..i] ])) (app (nameN "f" 1) [nat 2]) [2..size]
      ]
