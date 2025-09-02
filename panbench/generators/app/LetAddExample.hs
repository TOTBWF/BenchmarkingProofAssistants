{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module LetAddExample where

import Numeric.Natural

import Panbench.Generator
import Panbench


letAddExample
  :: (Module rep m, Import m "Data.Nat", Constant rep "Nat", Literal rep "Nat" Natural, Op2 rep "+")
  => GenModule Natural m rep
letAddExample = GenModule "LetAddExample" header body
  where
    header =
      import_ "Data.Nat"

    body size =
      defTm "n" (builtin "Nat") $
        let_ [SynLetDef (name "x" 0) [] (nat 1)] $
        let_ [SynLetDef (name "x" i) [] (op2 "+" (varN "x" (i - 1)) (varN  "x" (i - 1))) | i <- [1..size]] $
          var (name "x" size)
