{-# LANGUAGE GADTs, TypeOperators, PolyKinds, RankNTypes, CPP #-}

#include "macros.h"

LANGUAGE_TRUSTWORTHY

{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-orphans #-}

-- | Kind-polymorphic combinators for manipulating type equality evidence.
-- 
--   This module is available only if @PolyKinds@ are available (GHC 7.6+).

module Type.Eq.Poly (module Type.Eq, module Type.Eq.Poly) where

import Control.Applicative ((<$>))
import Control.Category ((.)) -- for haddock
import Data.Typeable hiding (cast)
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

fromEq1 :: f ::~:: g -> f :~: g
fromEq1 Eq1 = Eq

fromEq2 :: n :::~::: m -> n :~: m
fromEq2 Eq2 = Eq

fromOuterEq1 :: OuterEq1 m f -> OuterEq m f
fromOuterEq1 OuterEq1 = BUG_5591(OuterEq)

fromInnerEq1 :: InnerEq1 a f -> InnerEq a f
fromInnerEq1 InnerEq1 = BUG_5591(InnerEq)
