{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Extra
  where

import Data.Complex
import System.Random.MWC


data ArgType (a :: *) = AT

showType :: forall proxy a. Show (ArgType a) => proxy a -> String
showType _ = show (AT :: ArgType a)

instance Show (ArgType Float)            where show _ = "Float"
instance Show (ArgType Double)           where show _ = "Double"
instance Show (ArgType (Complex Float))  where show _ = "ComplexFloat"
instance Show (ArgType (Complex Double)) where show _ = "ComplexDouble"

instance Variate e => Variate (Complex e) where
  uniform    gen = (:+) <$> uniform gen <*> uniform gen
  uniformR r gen =
    let (ur:+ui,vr:+vi) = r
    in  (:+) <$> uniformR (ur,vr) gen <*> uniformR (ui,vi) gen

infixr 0 $$
($$) :: (b -> a) -> (c -> d -> b) -> c -> d -> a
(f $$ g) x y = f (g x y)

