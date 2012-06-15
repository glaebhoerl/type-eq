{-# LANGUAGE GADTs, TypeFamilies, KindSignatures, TypeOperators, CPP #-}

#include "macros.h"

LANGUAGE_TRUSTWORTHY

module Type.Eq.Higher where

data (f :: * -> *) ::~:: (g :: * -> *) where
    Eq1 :: (f ~ g) => f ::~:: g

data (m :: * -> * -> *) :::~::: (n :: * -> * -> *) where
    Eq2 :: (m ~ n) => m :::~::: n
