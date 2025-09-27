{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Panbench.Generator.SimpleDataDefinitions where

import Numeric.Natural

import Panbench.Generator
import Panbench


generator
  :: ( Module mod hdr defns
     , Import hdr "Data.Nat"
     , DataDefinition defns dataLhs ctor, ArgumentLhs dataLhsHd dataLhsArg dataLhs, Chk name tm dataLhsHd, Chk name tm ctor
     , Name name
     , Constant tm "Type"
     , Definition letDefn letLhs tm, ArgumentLhs letLhsHd letLhsArg letLhs, Syn nm letLhsHd, Chk name tm letLhsArg
     , Var nm tm, Let letDefn tm, App tm
     )
  => GenModule Natural hdr defns
generator =
  GenModule "DataSimpleDeclarations"
  [
  ] \size ->
  [ data_ ([] |- (nameN "X" i .: builtin "Type"))
    [ nameN "Y" i .: varN "X" i
    ]
  | i <- [1..size]
  ]
