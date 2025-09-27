module Panbench.Generator.SequentialDependentRecords where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule Natural hdr defns
generator =
  GenModule "ChainDef_DependentRecordModule"
  [
  ] \size ->
  [ recordN_ ([] |- nameN "Dummy" i .: builtin "Type") (nameN "Const" i) i \j ->
      nameN "f" j .: "Nat"
  | i <- [1..size]
  ] ++
  [ [] |- "exaample" .: nameN "Dummy" size .=
      foldr (\i tm -> app (nameN "Const" i) [tm]) (nat 10) [1..size]
  ]
