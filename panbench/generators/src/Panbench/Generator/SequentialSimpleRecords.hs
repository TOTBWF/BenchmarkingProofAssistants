module Panbench.Generator.SequentialSimpleRecords where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule Natural hdr defns
generator =
  GenModule "ChainDefFields_NonDependentRecordModule"
  [
  ] \size ->
  [ recordN_ ([] |- nameN "Dummy" i .: builtin "Type") (nameN "Const" i) i \j ->
      nameN "f" j .: builtin "Nat"
  | i <- [1..size]
  ] ++
  [ [] |- "example" .: nameN "Dummy" size .=
      foldr (\i tm -> app (nameN "Const" i) [tm]) (nat 10) [1..size]
  ]
