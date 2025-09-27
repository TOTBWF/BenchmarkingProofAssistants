{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.SequentialDefinitions where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Module mod hdr defns
     , Import hdr "Data.Nat"
     , Definition defns defnLhs tm, TelescopeLhs name tm defnLhs defnHd defnArg
     , DataDefinition defns dataLhs ctor, TelescopeLhs name tm dataLhs dataHd dataArg, Chk name tm ctor
     , Name name
     , Constant tm "Nat", Op2 tm "+", Literal tm "Nat" Natural
     , Constant tm "Type"
     , Var nm tm, App tm
     )
  => GenModule Natural hdr defns
generator =
    GenModule "FirstLast_VariableModule"
  [ import_ "Data.Nat"
  ] \size ->
  [ [] |- nameN "x" i .: builtin "Nat" .= nat i
  | i <- [1..size]
  ] ++
  [ [] |- "result" .: builtin "Nat" .= op2 "+" (varN "x" 1) (varN "x" size)
  ]
