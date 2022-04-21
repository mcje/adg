{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE GADTs #-}
import Data.List (union, elemIndex, length)
import Control.Parallel.Strategies

import Prelude hiding ((>>))

import qualified Fix (fix)

type Label = Int
type Name  = Int

-- Should we have renaming syntactically or functionally?

rename :: Expr -> Name -> Expr -> Expr
rename Nil _ _       = Nil
rename (Var y) x g   = if x == y then g else (Var y)
rename (Pre a e) x g = (Pre a (rename e x g))
rename (Sum e f) x g = (Sum (rename e x g) (rename f x g))
rename (Com e f) x g = (Com (rename e x g) (rename f x g))
rename (Fix y e) x g = (Fix y e') where
    e' = if x == y then (Fix y e) else (Fix y (rename e x g))

data Expr = Nil
          | Var Name
          | Pre Label Expr 
          | Sum Expr Expr 
          | Com Expr Expr
          | Fix Name Expr
          deriving Eq 

instance Show Expr where
    show Nil = "Nil"
    show (Var name) = "x" ++ show name
    show (Pre a e) = (show a) ++ "." ++ (show e)
    show (Sum e f) = (show e) ++ "+" ++ (show f)
    show (Com e f) = (show e) ++ "|" ++ (show f)
    show (Fix x e) = "u(" ++ show x ++ "=" ++ (show e) ++ ")"


getLabels :: Expr -> [Label]
getLabels Nil       = []
getLabels (Var _)   = []
getLabels (Pre a _) = [a]
getLabels (Sum e f) = union (getLabels e) (getLabels f)
getLabels (Com e f) = union (getLabels e) (getLabels f)
getLabels (Fix x e) = getLabels e

descendants :: Expr -> [Expr]
descendants Nil = [] 
descendants (Var v) = [] 
descendants (Pre a e) = union [e] (descendants e)
descendants (Sum e f) = union (descendants e) (descendants f)
descendants (Com e f) = union (descendants e) (descendants f)

sem :: Label -> Expr -> [Expr]
sem _ Nil               = []
sem a (Var name)       = []
sem a (Pre b e)   = if a == b then [e] else []
sem a (Sum e f) = union (sem a e) (sem a f)
sem 0 (Com e f) = union xs ys where
    xs = union [(Com e' f) | e' <- sem 0 e] [(Com e f') | f' <- sem 0 f]
    ys = [(Com e' f') | a <- getLabels e, e' <- sem a e, f' <- sem (-a) f]
sem a (Com e f) = union [(Com e' f) | e' <- sem a e] [(Com e f') | f' <- sem a f]
sem a (Fix x e) = sem a (rename e x (Fix x e))

type Graph n = n -> [[n]]
type Assignment n = n -> Bool

eval :: Graph n -> Assignment n -> Assignment n
eval g a n = and (map (or . (map a)) (g n)) 

simGraph :: Graph (Expr, Expr)
simGraph (e, f) = [[(e',f') | f' <- sem a f] | a <- getLabels e, e' <- sem a e]



p = Pre 1 (Var 0)
q = Sum (Pre 1 (Var 1)) (Pre 2 Nil)
graph = simGraph 
bottom _ = False
top _ = True

someP = union [p] (descendants p)
someQ = union [q] (descendants q)
prod = [(x,y) | x <- someP, y <- someQ]

instance Enum (Expr, Expr) where
    toEnum = (!!) prod 
    fromEnum e | elemIndex e prod /= Nothing = i where 
        (Just i) = elemIndex e prod

instance Bounded (Expr, Expr) where
    minBound = head prod 
    maxBound = last prod

x = Fix.fix (eval simGraph) bottom
