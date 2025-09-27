{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.LargeSimpleDatatype where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Module mod hdr defns
     , Import hdr "Data.Nat"
     , Definition defns defnLhs tm, TelescopeLhs name tm defnLhs defnHd defnArg
     , DataDefinition defns dataLhs ctor, TelescopeLhs name tm dataLhs dataHd dataArg, Chk name tm ctor
     , Name name
     , Constant tm "Nat", Op1 tm "suc", Literal tm "Nat" Natural
     , Constant tm "Type"
     , Var nm tm, App tm
     )
  => GenModule Natural hdr defns
generator =
  GenModule "Constructors_Datatypes"
  [ import_ "Data.Nat"
  ] \size ->
  [ dataN_ ([] |- "D" .: builtin "Type") size \i ->
    nameN "C" i .: var "D"
  ]
