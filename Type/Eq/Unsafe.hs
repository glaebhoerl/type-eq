{-# LANGUAGE TypeOperators, CPP #-}

#include "macros.h"

LANGUAGE_POLYKINDS
LANGUAGE_UNSAFE

-- | This module is kind-polymorphic if @PolyKinds@ are available (GHC 7.6+).

module Type.Eq.Unsafe where

import {-# SOURCE #-} Type.Eq
import Unsafe.Coerce

-- | Very unsafe! The same rules apply as for 'unsafeCoerce'.
unsafeCoercion :: a :~: b
unsafeCoercion = unsafeCoerce Eq
