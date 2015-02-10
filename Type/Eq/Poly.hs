{-# LANGUAGE GADTs, TypeOperators, PolyKinds, RankNTypes, CPP #-}

#include "macros.h"

LANGUAGE_TRUSTWORTHY
LANGUAGE_AUTODERIVETYPEABLE

{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-orphans #-}

-- | Kind-polymorphic functions for manipulating type equality evidence.
-- 
--   This module is available only if @PolyKinds@ are available (GHC 7.6+).

module Type.Eq.Poly (module Type.Eq, module Type.Eq.Poly) where

import Control.Applicative ((<$>))
import Control.Category ((.)) -- for haddock
import Data.Typeable (Typeable1, typeOf1, Typeable2, typeOf2, Typeable3, typeOf3, Typeable4, typeOf4, Typeable5, typeOf5, Typeable6, typeOf6, Typeable7, typeOf7)
import Type.Eq
import Type.Eq.Higher ((::~::)(..), (:::~:::)(..), OuterEq1(..), InnerEq1(..))
import Type.Eq.Unsafe
import Prelude hiding ((.))
import Unsafe.Coerce

{-
INSTANCE_TYPEABLE(1,:~:,f,g,"Type.Eq",":~:",())
INSTANCE_TYPEABLE(2,:~:,m,n,"Type.Eq",":~:",() ())
INSTANCE_TYPEABLE(3,:~:,x,y,"Type.Eq",":~:",() () ())
INSTANCE_TYPEABLE(4,:~:,x,y,"Type.Eq",":~:",() () () ())
INSTANCE_TYPEABLE(5,:~:,x,y,"Type.Eq",":~:",() () () () ())
INSTANCE_TYPEABLE(6,:~:,x,y,"Type.Eq",":~:",() () () () () ())
INSTANCE_TYPEABLE(7,:~:,x,y,"Type.Eq",":~:",() () () () () () ())
-}

-- | Synonym for @'composeEq'@. Kind-polymorphic, unlike @('.')@.
(|.|) :: b :~: c -> a :~: b -> a :~: c
(|.|) = composeEq

-- | Congruence?
applyEq, (|$|) :: f :~: g -> a :~: b -> f a :~: g b
applyEq = withEq (withEq Eq)
(|$|) = applyEq

-- | Type constructors are generative
constructorEq :: f a :~: g b -> f :~: g
constructorEq = withEq BUG_5591(Eq)

DYNAMIC_EQ(1,,:~:,f,g,())
DYNAMIC_EQ(2,,:~:,n,m,() ())
DYNAMIC_EQ(3,,:~:,x,y,() () ())
DYNAMIC_EQ(4,,:~:,x,y,() () () ())
DYNAMIC_EQ(5,,:~:,x,y,() () () () ())
DYNAMIC_EQ(6,,:~:,x,y,() () () () () ())
DYNAMIC_EQ(7,,:~:,x,y,() () () () () () ())

sameOuterEq :: OuterEq f a -> OuterEq g a -> f :~: g
sameOuterEq OuterEq OuterEq = BUG_5591(Eq)

-- * Compatibility with Type.Eq.Higher

fromEq1 :: f ::~:: g -> f :~: g
fromEq1 Eq1 = Eq

toEq1 :: f :~: g -> f ::~:: g
toEq1 Eq = Eq1

fromEq2 :: n :::~::: m -> n :~: m
fromEq2 Eq2 = Eq

toEq2 :: n :~: m -> n :::~::: m
toEq2 Eq = Eq2

fromOuterEq1 :: OuterEq1 m f -> OuterEq m f
fromOuterEq1 OuterEq1 = BUG_5591(OuterEq)

toOuterEq1 :: OuterEq m f -> OuterEq1 m f
toOuterEq1 OuterEq = OuterEq1

fromInnerEq1 :: InnerEq1 a f -> InnerEq a f
fromInnerEq1 InnerEq1 = BUG_5591(InnerEq)

toInnerEq1 :: InnerEq a f -> InnerEq1 a f
toInnerEq1 InnerEq = InnerEq1
