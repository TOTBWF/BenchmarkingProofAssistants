{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.SequentialSimpleRecords where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Module mod hdr defns
     , Import hdr "Data.Nat"
     , Definition defns defnLhs tm, TelescopeLhs name tm defnLhs defnHd defnArg
     , RecordDefinition defns recordLhs name field, TelescopeLhs name tm recordLhs recordHd recordLhsArg
     , Chk name tm field
     , Name name
     , Constant tm "Nat", Literal tm "Nat" Natural
     , Constant tm "Type"
     , Var nm tm, App tm
     )
  => GenModule Natural hdr defns
generator =
  GenModule "ChainDefFields_NonDependentRecordModule"
  [
  ] \size ->
  [ recordN_ ([] |- nameN "Dummy" i .: builtin "Type") (nameN "Const" i) i \j ->
      nameN "f" j .: var "Nat"
  | i <- [1..size]
  ] ++
  [ [] |- "example" .: varN "Dummy" size .=
      foldr (\i tm -> app (varN "Const" i) [tm]) (nat 10) [1..size]
  ]
