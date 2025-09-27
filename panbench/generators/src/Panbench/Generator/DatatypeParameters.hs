{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.DatatypeParameters where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator
  :: ( Module mod hdr defns
     , Import hdr "Data.Nat"
     , Definition defns defnLhs tm, TelescopeLhs name tm defnLhs defnHd defnArg
     , DataDefinition defns dataLhs ctor, TelescopeLhs name tm dataLhs dataHd dataArg, Chk name tm ctor
     , Name name
     , Constant tm "Type"
     , Var nm tm, App tm
     )
  => GenModule Natural hdr defns
generator =
  GenModule "Parameters_Datatypes"
  [
  ] \size ->
  [ data_ ([ nameN "p" i .: builtin "Type" | i <- [1..size]] |- "D" .: builtin "Type")
    [ "C" .: app (var "D") [ varN "p" i | i <- [1..size] ]
    ]
  ]
