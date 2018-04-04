{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Bench.Util
-- Copyright   : [2017] Trevor L. McDonell
-- License     : BSD3
--
-- Maintainer  : Trevor L. McDonell <tmcdonell@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Bench.Util where

import Data.Complex
import System.Random.MWC


data ArgType (a :: *) = AT

showType :: forall proxy a. Show (ArgType a) => proxy a -> String
showType _ = show (AT :: ArgType a)

instance Show (ArgType Float)            where show _ = "Float"
instance Show (ArgType Double)           where show _ = "Double"
instance Show (ArgType (Complex Float))  where show _ = "Complex Float"
instance Show (ArgType (Complex Double)) where show _ = "Complex Double"

instance Variate e => Variate (Complex e) where
  uniform    gen = (:+) <$> uniform gen <*> uniform gen
  uniformR r gen =
    let (ur:+ui,vr:+vi) = r
    in  (:+) <$> uniformR (ur,vr) gen <*> uniformR (ui,vi) gen

infixr 0 $$
($$) :: (b -> a) -> (c -> d -> b) -> c -> d -> a
(f $$ g) x y = f (g x y)

