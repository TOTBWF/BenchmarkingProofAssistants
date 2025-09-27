{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.LargeDependentRecord where

import Numeric.Natural

import Panbench.Generator
import Panbench


generator
  :: ( Module mod hdr defns
     , Import hdr "Data.Nat"
     , Postulate defns postulateLhs, TelescopeLhs name tm postulateLhs postulateHd postulateArg
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
  GenModule "Fields_DependentRecordModule"
  [
  ] \size ->
  [ postulate (["n" .: builtin "Nat"] |- ("P" .: builtin "Type"))
  , postulate ([] |- "nil" .: app (var "P") [nat 0])
  , postulate (["n" .: builtin "Nat", "xs" .: app (var "P") [var "n"]] |- ("cons" .: app (var "P") [op1 "suc" (var "n")]))
  , record_ ([] |- "Cap_X" .: builtin "Type") "Const" $
      (nameN "f" 1 .: builtin "Nat") :
      [ nameN "f" i .: app (var "P") [iter (op1 "suc") (varN "f" 1) (i - 1)]
      | i <- [2..size]
      ]
  -- [TODO: Reed M, 26/09/2025] Should we use a record constructor literal here?
  , ([] |- "example" .: var "Cap_X") .=
      app (var "Const")
        [ foldr (\j tm -> app (var "cons") [nat j, tm]) (var "nil") [0..i-1]
        | i <- [1..size]
        ]
  ]
