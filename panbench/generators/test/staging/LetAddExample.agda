module LetAddExample where
open import Agda.Builtin.Nat
n : Nat
n =
  let x0 = 1 in
  let x1 = x0 + x0
      x2 = x1 + x1
      x3 = x2 + x2
      x4 = x3 + x3
      x5 = x4 + x4
  in x5
