{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
-- | Panbench utilities.
module Panbench
  ( module Panbench.Grammar
  , iter
  ) where


import Numeric.Natural

import Panbench.Grammar

iter :: (a -> a) -> a -> Natural -> a
iter s z 0 = z
iter s z n = s (iter s z (n - 1))
