{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
import Data.Finite

import qualified Fix (Stop, stop, fix, fix_stop)


type HyperGraph n = n -> [[n]]
type Assignment n = n -> Bool


eval :: HyperGraph n -> Assignment n -> Assignment n
eval g a n = or (map (and . (map a)) (g n)) 

bottom :: Assignment n
bottom _ = False 


-- Example
type Nodes = Finite 5

graph :: HyperGraph Nodes
graph 0 = [[], [1, 2]]
graph 1 = []
graph 2 = [[3]]
graph 3 = [[]]
graph 4 = [[2]]

a = Fix.fix (eval graph) bottom

instance Fix.Stop (Assignment Nodes) where
    stop a = a 0

a' = Fix.fix_stop (eval graph) bottom
