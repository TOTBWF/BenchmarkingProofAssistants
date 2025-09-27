{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.RecordParameters where

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
     , Constant tm "Nat", Literal tm "Nat" Natural, Op2 tm "+"
     , Constant tm "Type"
     , Var nm tm, App tm
     )
  => GenModule Natural hdr defns
generator =
  GenModule "Parameters_DependentRecordModule"
  [
  ] \size ->
  [ record_ ([ nameN "f" i .: builtin "Nat" | i <- [1..size]] |- "X" .: builtin "Type") "Const"
      [ "sums" .: builtin "Nat"
      ]
  -- [FIXME: Reed M, 26/09/2025] This is a bad benchmark, and we shouldn't be using addition here.
  , [] |- "example" .: app (var "X") [nat i | i <- [1..size]] .=
      app (var "Const") [foldl (\tm i -> op2 "+" tm (nat i)) (nat 1) [2..size]]
  ]
