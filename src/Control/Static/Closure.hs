{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StaticPointers #-}
{-|
This module contains a "closure" type, originally inspired by
'Control.Distributed.Closure.Internal.Closure', but modified significantly,
and also generalised in a number of ways.

There was some also unsafe casts in "Control.Distributed.Closure.Internal" that scared me,
which this package does not have.
-}
module Control.Static.Closure where

import Control.Static.Closure.IsClosure (IsClosure(closure, unclosure, cap))
import Control.Static.Closure.IsPureClosure (IsPureClosure(cpure, ClosureConstraint))
import Control.Static.Closure.HasClosureDict (HasClosureDict(getClosureDict))

import GHC.StaticPtr (IsStatic(fromStaticPtr), StaticPtr, deRefStaticPtr, staticKey, unsafeLookupStaticPtr)

import Data.Binary (Put, Get, Binary(put, get), encode, decode)
import Data.Word (Word8)

import Data.Constraint (Dict(Dict))

import qualified Data.ByteString.Lazy as BSL

import System.IO.Unsafe (unsafePerformIO)
import Data.Typeable (Typeable)

{-|
Somewhat inspired by 'Control.Distributed.Closure.Internal.Closure'
but modified. Whereas 'Control.Distributed.Closure.Internal.Closure' requires
the serialised type to be a lazy 'Data.ByteString.Lazy.ByteString', this closure
type allows the serialised type to be given as a parameter.
-}
data Closure t a where
  CPure :: !(Closure t (t -> a)) -> t -> a -> Closure t a
  CStaticPtr :: !(StaticPtr a) -> Closure t a
  CAp :: !(Closure t (a -> b)) -> !(Closure t a) -> Closure t b

instance IsClosure (Closure t) where
  closure = CStaticPtr
  unclosure = \case
    CPure _ _ x -> x
    CStaticPtr p -> deRefStaticPtr p
    CAp c1 c2 -> (unclosure c1) (unclosure c2)
  cap = CAp

decodeWithDict :: Dict (Binary a) -> BSL.ByteString -> a
decodeWithDict Dict = decode

instance IsPureClosure (Closure BSL.ByteString) where
  type ClosureConstraint (Closure BSL.ByteString) a = (Typeable a, HasClosureDict (Binary a))
  {-|
  Inspired by 'Control.Distributed.Closure.Internal.cpure'.
  -}
  cpure :: forall a. (Typeable a, HasClosureDict (Binary a)) => a -> Closure BSL.ByteString a
  cpure = go getClosureDict where
    go :: Closure BSL.ByteString (Dict (Binary a)) -> a -> Closure BSL.ByteString a
    go closureDict x = CPure f (encode x) x where
      f = static decodeWithDict `cap` closureDict

instance IsStatic (Closure t) where
  fromStaticPtr = CStaticPtr

instance Binary t => Binary (Closure t a) where
  put = putClosure
  get = getClosure

newtype Tag = Tag Word8 deriving Binary

pattern PureTag :: Tag
pattern PureTag = (Tag 0)

pattern StaticPtrTag :: Tag
pattern StaticPtrTag = Tag 1

pattern ApTag :: Tag
pattern ApTag = Tag 2

{-|
Inspired by 'Control.Distributed.Closure.Internal.putClosure'.
-}
putClosure :: Binary t => Closure t a -> Put
putClosure (CPure c bs _) = put PureTag >> put bs >> putClosure c
putClosure (CStaticPtr p) = put StaticPtrTag >> put (staticKey p)
putClosure (CAp c1 c2) = put ApTag >> putClosure c1 >> putClosure c2
{-|
Inspired by 'Control.Distributed.Closure.Internal.getDynClosure',
but I think simplified.
-}
getClosure :: Binary t => Get (Closure t a)
getClosure = get >>= \case
  PureTag -> do
    bs <- get
    c <- getClosure
    let x = (unclosure c) bs
    pure $ CPure c bs x
  StaticPtrTag -> get >>= \key -> case unsafePerformIO (unsafeLookupStaticPtr key) of
    Just sptr -> pure $ CStaticPtr sptr
    Nothing -> fail $ "Static pointer lookup failed: " ++ show key
  ApTag -> CAp <$> getClosure <*> getClosure
  _ -> fail "Binary.get(Closure): unrecognized tag."
