{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, CPP #-}

#include "macros.h"

LANGUAGE_POLYKINDS
LANGUAGE_TRUSTWORTHY

module Type.Eq where

data a :~: b where
    Eq :: (a ~ b) => a :~: b

data OuterEq f a where
    OuterEq :: f i ~ a => OuterEq f a

data InnerEq i a where
    InnerEq :: f i ~ a => InnerEq i a
