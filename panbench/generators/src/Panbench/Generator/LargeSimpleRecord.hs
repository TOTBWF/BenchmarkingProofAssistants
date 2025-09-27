{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.LargeSimpleRecord where

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
     , Constant tm "Nat", Op1 tm "suc", Literal tm "Nat" Natural
     , Constant tm "Type"
     , Var nm tm, App tm
     )
  => GenModule Natural hdr defns
generator =
  GenModule "Fields_NonDependentRecordModule"
  [ import_ "Data.Nat"
  ] \size ->
  [ recordN_ ([] |- "Cap_X" .: builtin "Type") "Const" size \i ->
      nameN "f" i .: builtin "Nat"
  -- [TODO: Reed M, 26/09/2025] Should we use a record constructor literal here?
  , ([] |- "example" .: var "Cap_X") .=
      app (var "Const")
        [ nat 1
        | _ <- [1..size]
        ]
  ]
