{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}

module Fix where

import Prelude hiding ((<=))
import Data.PartialOrd (PartialOrd, (<=))
import Data.Finite 
import GHC.TypeNats


-- Constraint type describing wether a generic type is finite
-- Why say that something is finite iff it is enumerable and bounded
type IsFinite a = (Enum a, Bounded a)

instance (IsFinite a, Eq b) => Eq (a -> b) where 
    f == g = and $ zipWith (==) (map f is) (map g is) where 
        is = enumFromTo minBound maxBound 

--instance (Only a, Eq b) => Eq (a -> b) where
--    f == g = and $ zipWith (==) (map f only) (map g only)

instance (IsFinite a, PartialOrd b) => PartialOrd (a -> b) where 
    f <= g = and $ zipWith (<=) (map f is) (map g is) where 
        is = enumFromTo minBound maxBound 


valuepairs :: (IsFinite a) => (a -> b) -> [(a,b)]
valuepairs f = zip domain codomain where
    domain = enumFromTo minBound maxBound
    codomain = map f (enumFromTo minBound maxBound)

instance (IsFinite a, Show a, Show b) => Show (a -> b) where
    show f = show (map show (valuepairs f))

  
-- returns the fixed point of a function f :: (a -> a) from start x :: a
fix :: Eq a => (a -> a) -> a -> a
fix f x = if    x == fx
          then  x
          else  fix f fx
          where fx = f x

class Stop a where 
    stop :: a -> Bool

-- stops the fixed point calculation early based on some inherent Stop property of a
fix_stop :: (Stop a) => (a -> a) -> a -> a
fix_stop f a = if    stop a
               then  a
               else  fix_stop f a'
               where a' = f a


