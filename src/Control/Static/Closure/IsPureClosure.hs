{-# LANGUAGE TypeFamilies #-}

module Control.Static.Closure.IsPureClosure where

import Control.Static.Closure.IsClosure (IsClosure(unclosure))
import GHC.Exts (Constraint)

class IsClosure t => IsPureClosure t where
  type ClosureConstraint t a :: Constraint
  cpure :: ClosureConstraint t a => a -> t a
  cfmap :: ClosureConstraint t b => (a -> b) -> t a -> t b
  cfmap f = cpure . f . unclosure
