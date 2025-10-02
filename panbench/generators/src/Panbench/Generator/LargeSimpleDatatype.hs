module Panbench.Generator.LargeSimpleDatatype where

import Numeric.Natural

import Panbench.Generator
import Panbench

generator :: _ => GenModule Natural hdr defns
generator =
  GenModule "Constructors_Datatypes"
  [
  ] \size ->
  [ dataN_ ([] |- "D" .: builtin "Type") size \i ->
    nameN "C" i .: "D"
  ]
