{-# LANGUAGE TypeOperators, CPP #-}

#include "macros.h"

LANGUAGE_UNSAFE

module Type.Eq.Higher.Unsafe (module Type.Eq.Unsafe, module Type.Eq.Higher.Unsafe) where

import Type.Eq.Unsafe
import {-# SOURCE #-} Type.Eq.Higher
import Unsafe.Coerce

-- | Very unsafe! The same rules apply as for 'unsafeCoerce'.
unsafeCoercion1 :: f ::~:: g
unsafeCoercion1 = unsafeCoerce Eq1

-- | Very unsafe! The same rules apply as for 'unsafeCoerce'.
unsafeCoercion2 :: m :::~::: n
unsafeCoercion2 = unsafeCoerce Eq2

-- | Very unsafe!
unsafeOuterEq1 :: OuterEq1 m f
unsafeOuterEq1 = unsafeCoerce OuterEq1

-- | Very unsafe!
unsafeInnerEq1 :: InnerEq1 a f
unsafeInnerEq1 = unsafeCoerce InnerEq1
