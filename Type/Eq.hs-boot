{-# LANGUAGE GADTs, TypeFamilies, TypeOperators, CPP #-}

#include "macros.h"

LANGUAGE_POLYKINDS
LANGUAGE_TRUSTWORTHY

module Type.Eq where

data a :~: b where
    Eq :: (a ~ b) => a :~: b
