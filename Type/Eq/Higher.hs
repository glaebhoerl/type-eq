{-# LANGUAGE GADTs, TypeFamilies, Rank2Types, KindSignatures, TypeOperators, FlexibleContexts, CPP #-}

#include "macros.h"

LANGUAGE_TRUSTWORTHY

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |  Types and functions for storing and manipulating evidence of equality between types of higher kind.
-- 
--    Available up to @* -> * -> *@. Yell if you need more.
-- 
--    In GHC 7.8, this module uses @Data.OldTypeable@. Future uncertain.

module Type.Eq.Higher (module Type.Eq, module Type.Eq.Higher) where

#if __GLASGOW_HASKELL__ >= 707
import Data.OldTypeable hiding (cast)
#else
import Data.Typeable hiding (cast)
#endif
import Type.Eq
import Type.Eq.Higher.Unsafe
import Unsafe.Coerce

-- * Additional functions, kind @*@

-- | Type constructors are generative
constructorEq :: f a :~: g b -> f ::~:: g
constructorEq Eq = BUG_5591(Eq1)

sameOuterEq :: OuterEq f a -> OuterEq g a -> f ::~:: g
sameOuterEq OuterEq OuterEq = BUG_5591(Eq1)


-- * Full equality, kind @* -> *@

data (f :: * -> *) ::~:: (g :: * -> *) where
    Eq1 :: (f ~ g) => f ::~:: g

-- INSTANCE_TYPEABLE(1,::~::,f,g,"Type.Eq.Higher","::~::",())

withEq1 :: (f ~ g => r) -> (f ::~:: g) -> r
withEq1 x Eq1 = x

-- | Reflexivity
idEq1 :: f ::~:: f
idEq1 = Eq1

-- | Transitivity
composeEq1, (|.|) :: (g ::~:: h) -> (f ::~:: g) -> (f ::~:: h)
composeEq1 Eq1 Eq1 = Eq1
(|.|) = composeEq1

-- | Symmetry
flipEq1 :: (f ::~:: g) -> (g ::~:: f)
flipEq1 Eq1 = Eq1

-- | Congruence?
applyEq1, (|$|) :: f ::~:: g -> a :~: b -> f a :~: g b
applyEq1 Eq1 Eq = Eq
(|$|) = applyEq1

-- | Type constructors are generative
constructorEq1 :: m a ::~:: n b -> m :::~::: n
constructorEq1 Eq1 = BUG_5591(Eq2)

-- | Type constructors are injective
argumentEq1 :: m a ::~:: n b -> a :~: b
argumentEq1 Eq1 = BUG_5591(Eq)

DYNAMIC_EQ(1,1,::~::,f,g,())


-- * Partial equality, kind @* -> *@

data OuterEq1 (m :: * -> * -> *) (f :: * -> *) where
    OuterEq1 :: m a ~ f => OuterEq1 m f

data InnerEq1 (a :: *) (f :: * -> *) where
    InnerEq1 :: m a ~ f => InnerEq1 a f

withOuterEq1 :: (forall a. m a ~ f => r) -> OuterEq1 m f -> r
withOuterEq1 a OuterEq1 = a

withInnerEq1 :: (forall m. m a ~ f => r) -> InnerEq1 a f -> r
withInnerEq1 a InnerEq1 = a

outerEq1 :: m a ::~:: f -> OuterEq1 m f
outerEq1 Eq1 = OuterEq1

innerEq1 :: m a ::~:: f -> InnerEq1 a f
innerEq1 Eq1 = InnerEq1

assembleEq1 :: OuterEq1 m f -> InnerEq1 a f -> m a ::~:: f
assembleEq1 OuterEq1 InnerEq1 = BUG_5591(Eq1)

sameOuterEq1 :: OuterEq1 m f -> OuterEq1 n f -> m :::~::: n
sameOuterEq1 OuterEq1 OuterEq1 = BUG_5591(Eq2)

sameInnerEq1 :: InnerEq1 a f -> InnerEq1 b f -> a :~: b
sameInnerEq1 InnerEq1 InnerEq1 = BUG_5591(Eq)


-- * Full equality, kind @* -> * -> *@

data (m :: * -> * -> *) :::~::: (n :: * -> * -> *) where
    Eq2 :: (m ~ n) => m :::~::: n

-- INSTANCE_TYPEABLE(2,:::~:::,m,n,"Type.Eq.Higher",":::~:::",() ())

withEq2 :: (m ~ n => r) -> (m :::~::: n) -> r
withEq2 x Eq2 = x

-- | Reflexivity
idEq2 :: m :::~::: m
idEq2 = Eq2

-- | Transitivity
composeEq2, (||.||) :: (n :::~::: o) -> (m :::~::: n) -> (m :::~::: o)
composeEq2 Eq2 Eq2 = Eq2
(||.||) = composeEq2

-- | Symmetry
flipEq2 :: (m :::~::: n) -> (n :::~::: m)
flipEq2 Eq2 = Eq2

-- | Congruence?
applyEq2, (||$||) :: m :::~:::n -> a :~: b -> m a ::~:: n b
applyEq2 Eq2 Eq = Eq1
(||$||) = applyEq2

DYNAMIC_EQ(2,2,:::~:::,n,m,() ())
