module Control.Static.Closure.IsClosure where

import GHC.StaticPtr (StaticPtr, IsStatic)

class IsStatic t => IsClosure t where
  closure :: StaticPtr a -> t a
  unclosure :: t a -> a
  cap :: t (a -> b) -> t a -> t b
  cmap :: StaticPtr (a -> b) -> t a -> t b
  cmap = cap . closure
