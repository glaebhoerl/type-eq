{-# LANGUAGE GADTs, TypeFamilies, KindSignatures, TypeOperators, CPP #-}

#include "macros.h"

LANGUAGE_TRUSTWORTHY

module Type.Eq.Higher where

data (f :: * -> *) ::~:: (g :: * -> *) where
    Eq1 :: (f ~ g) => f ::~:: g

data (m :: * -> * -> *) :::~::: (n :: * -> * -> *) where
    Eq2 :: (m ~ n) => m :::~::: n

data OuterEq1 (m :: * -> * -> *) (f :: * -> *) where
    OuterEq1 :: m a ~ f => OuterEq1 m f

data InnerEq1 (a :: *) (f :: * -> *) where
    InnerEq1 :: m a ~ f => InnerEq1 a f
