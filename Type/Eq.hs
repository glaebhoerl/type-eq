{-# LANGUAGE GADTs, TypeFamilies, Rank2Types, TypeOperators, FlexibleInstances, FlexibleContexts, CPP #-}

#include "macros.h"

LANGUAGE_POLYKINDS
LANGUAGE_TRUSTWORTHY

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Types and functions for storing and manipulating type equality evidence.
-- 
--   This module is kind-polymorphic if @PolyKinds@ are available (GHC 7.6+).
--
--   Notable functions missing from this module include @applyEq@, @constructorEq@, and @sameOuterEq@.
-- 
--   See also @"Type.Eq.Higher"@ and @"Type.Eq.Poly"@.

module Type.Eq where

import Control.Category         (Category(..))
import Control.Applicative      (Applicative) -- for haddock
--import Control.Category.Product (Tensor(..))
--import Data.Groupoid            (Groupoid(..))
--import Data.Semigroupoid        (Semigroupoid(..))
import Data.Typeable     hiding (cast)
import Type.Eq.Unsafe
import Prelude           hiding ((.))
import Unsafe.Coerce

-- * Full equality

-- | Evidence that @a@ is the same type as @b@.
-- 
--   The @'Functor'@, @'Applicative'@, and @'Monad'@ instances of @Maybe@
--   are very useful for working with values of type @Maybe (a :~: b)@.
data a :~: b where
    Eq :: (a ~ b) => a :~: b

-- FIXME ifdef this for 7.8
-- deriving Typeable and PolyKinds don't play well
instance Typeable2 (:~:) where
    typeOf2 = const $ mkTyConApp tyCon []
        where tyCon = MK_TY_CON("Type.Eq",":~:")

-- | Unpack equality evidence and use it.
-- 
--   This function compiles with GHC 6.10, but doesn't work.
withEq :: (a ~ b => r) -> (a :~: b) -> r
withEq x Eq = x
-- for compatibility, we're not going to use it either

instance Category (:~:) where
    id  = idEq
    (.) = composeEq

{-
instance Semigroupoid (:~:) where
    o = composeEq

instance Groupoid (:~:) where
    inv = flipEq

instance Tensor (:~:) where
    a *** b = idEq2 ||$|| a |$| b
-}

-- | Reflexivity
idEq :: a :~: a
idEq = Eq

-- | Transitivity
composeEq :: (b :~: c) -> (a :~: b) -> (a :~: c)
composeEq Eq Eq = Eq

-- | Symmetry
flipEq :: (a :~: b) -> (b :~: a)
flipEq Eq = Eq

-- | Type constructors are injective
argumentEq :: (f a :~: g b) -> (a :~: b)
argumentEq Eq = BUG_5591(Eq)

-- | Use equality evidence to cast between types
cast, (|>) :: a -> (a :~: b) -> b
cast a Eq = a
(|>) = cast

-- * Partial equality

-- | Evidence that @f@ is the outermost type constructor of @a@
data OuterEq f a where
    OuterEq :: f i ~ a => OuterEq f a

-- | Evidence that @i@ is the argument type of the outermost type constructor of @a@
data InnerEq i a where
    InnerEq :: f i ~ a => InnerEq i a

-- | Unpack partial equality evidence and use it.
--
--   This function compiles with GHC 6.10, but doesn't work.
withOuterEq :: (forall i. f i ~ a => r) -> OuterEq f a -> r
withOuterEq x OuterEq = x

-- | Unpack partial equality evidence and use it.
--
--   This function compiles with GHC 6.10, but doesn't work.
withInnerEq :: (forall f. f i ~ a => r) -> InnerEq i a -> r
withInnerEq x InnerEq = x

outerEq :: f i :~: a -> OuterEq f a
outerEq Eq = OuterEq

innerEq :: f i :~: a -> InnerEq i a
innerEq Eq = InnerEq

assembleEq :: OuterEq f a -> InnerEq i a -> f i :~: a
assembleEq OuterEq InnerEq = BUG_5591(Eq)

sameInnerEq :: InnerEq i a -> InnerEq j a -> i :~: j
sameInnerEq InnerEq InnerEq = BUG_5591(Eq)

-- * Testing for equality

dynamicEq :: (Typeable a, Typeable b) => Maybe (a :~: b)
dynamicEq = gcast idEq

--dynamicInnerEq :: (Typeable (f i), Typeable a) => Maybe (InnerEq i a)
--dynamicOuterEq :: (Tyepable (f i), Typeable a) => Maybe (OuterEq f a)

--data Type a where
--    Type :: Typeable a => Type a

-- | Can be implemented by types storing evidence of type equalities, i.e. GADTs.
-- 
--   A return value of @Nothing@ can mean any of definite inequality, impossible arguments, or insufficient information.
--
--   Minimal complete definition: @maybeEq@ or @(~~)@, plus either:
-- 
--       - @piecewiseMaybeEq@, or
-- 
--       - both @maybeOuterEq@ and @maybeInnerEq@. or
-- 
--       - @(\<~>)@, or
-- 
--       - both @(~>)@ and @(<~)@.
-- 
--   Due to <http://hackage.haskell.org/trac/ghc/ticket/5591> you may have to use 'unsafeOuterEq' and/or 'unsafeInnerEq' to define some of these.
class TypeCompare t where
    maybeEq,          (~~)  :: t a     -> t b -> Maybe (a :~: b)
    maybeOuterEq,     (~>)  :: t (f i) -> t a -> Maybe (OuterEq f a)
    maybeInnerEq,     (<~)  :: t (f i) -> t a -> Maybe (InnerEq i a)
    piecewiseMaybeEq, (<~>) :: t (f i) -> t a -> (Maybe (OuterEq f a), Maybe (InnerEq i a))
    -- ^ > uncurry (liftA2 assembleEq) (a <~> b) = a ~~ b
    maybeEq = (~~)
    (~~)    = maybeEq
    maybeOuterEq     a b = fst (a <~> b)
    maybeInnerEq     a b = snd (a <~> b)
    piecewiseMaybeEq a b = (a ~> b, a <~ b)
    (~>)  = maybeOuterEq
    (<~)  = maybeInnerEq
    (<~>) = piecewiseMaybeEq

instance TypeCompare ((:~:) a) where
    maybeEq      Eq Eq = Just Eq
    maybeOuterEq Eq Eq = Just OuterEq
    maybeInnerEq Eq Eq = Just InnerEq

instance TypeCompare (InnerEq i) where
    maybeEq      _ _ = Nothing
    maybeOuterEq _ _ = Nothing
    maybeInnerEq InnerEq InnerEq = Just BUG_5591(InnerEq)

instance TypeCompare (OuterEq f) where
    maybeEq      _ _ = Nothing
    maybeOuterEq OuterEq OuterEq = Just BUG_5591(OuterEq)
    maybeInnerEq _ _ = Nothing
