#ifndef TYPE_EQ_MACROS_H
#define TYPE_EQ_MACROS_H

-- NT: TypeableN
-- NC: unsafeCoercionN
-- EQ: :~:, ::~::, ...
-- T1, T2: a, b; f, g; ...
-- ARGS: () ()...
#define DYNAMIC_EQ(NT, NC, EQ, T1, T2, ARGS)                                                         \
    ; {- | Runtime type equality evidence from @Typeable/**/NT@ -};                                  \
    dynamicEq/**/NT :: (Typeable/**/NT T1, Typeable/**/NT T2) => Maybe (T1 EQ T2);                   \
    dynamicEq/**/NT = r where {                                                                      \
        r       = if typeOf/**/NT T1 == typeOf/**/NT T2 then Just unsafeCoercion/**/NC else Nothing; \
        T1      = x/**/T1 r;                                                                         \
        T2      = x/**/T2 r;                                                                         \
        x/**/T1 :: Maybe (T1 EQ T2) -> T1 ARGS;                                                      \
        x/**/T1 = const undefined;                                                                   \
        x/**/T2 :: Maybe (T1 EQ T2) -> T2 ARGS;                                                      \
        x/**/T2 = const undefined;                                                                   \
    }

#define INSTANCE_TYPEABLE(NT, EQ, T1, T2, MOD, CON, ARGS)                                            \
    instance (Typeable/**/NT T1, Typeable/**/NT T2) => Typeable (T1 EQ T2) where                     \
        typeOf a = tyCon `mkTyConApp` (map unapply [typeOf/**/NT (type1 a), typeOf/**/NT (type2 a)]) \
            where {                                                                                  \
                tyCon = MK_TY_CON(MOD, CON);                                                         \
                unapply t = mkTyConApp (typeRepTyCon t) [];                                          \
                type1 :: T1 EQ T2 -> T1 ARGS;                                                        \
                type1 = const undefined;                                                             \
                type2 :: T1 EQ T2 -> T2 ARGS;                                                        \
                type2 = const undefined;                                                             \
            }

#if MIN_VERSION_base(4,4,0)
#   define MK_TY_CON(MOD, CON) mkTyCon3 "type-eq" MOD CON
#else
#   define MK_TY_CON(MOD, CON) mkTyCon ("type-eq::" ++ MOD ++ "." ++ CON)
#endif

-- http://hackage.haskell.org/trac/ghc/ticket/5591
#if (__GLASGOW_HASKELL__ >= 702) && (__GLASGOW_HASKELL__ <= 704)
#   define BUG_5591(X) (unsafeCoerce X)
#else
#   define BUG_5591(X) X
#endif

#if (__GLASGOW_HASKELL__ >= 702)
#   define LANGUAGE_TRUSTWORTHY {-# LANGUAGE Trustworthy #-}
#else
#   define LANGUAGE_TRUSTWORTHY
#endif

#if (__GLASGOW_HASKELL__ >= 704)
#   define LANGUAGE_UNSAFE    {-# LANGUAGE Unsafe #-}
#else
#   define LANGUAGE_UNSAFE
#endif

#if (__GLASGOW_HASKELL__ >= 706)
#   define LANGUAGE_POLYKINDS {-# LANGUAGE PolyKinds #-}
#else
#   define LANGUAGE_POLYKINDS
#endif

#endif
