{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE GADTs #-}

data Expr where
    Nil :: Expr
    Var :: Int -> Expr 
    Pre :: Int -> Expr -> Expr 
    Sum :: Expr -> Expr -> Expr
    Com :: Expr -> Expr -> Expr
    Fix :: Int -> Expr -> Expr
