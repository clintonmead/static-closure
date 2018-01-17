{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.Static.Closure.HasClosureDict where

import Control.Static.Closure.IsClosure (IsClosure)
import Data.Constraint (Dict)
import Data.Binary (Binary)

import Control.Instances.GHC_Packages ()

import Control.Static.Closure.TH (mkAllInstances)

class c => HasClosureDict c where
  getClosureDict :: IsClosure t => t (Dict c)
{-
instance Static (Binary Int) where
  getClosureDict = static Dict

instance (Typeable a, Static (Binary a)) => Static (Binary [a]) where
  getClosureDict = (static (\Dict -> Dict)) `cap` (getClosureDict :: IsClosure t => t (Dict (Binary a)))
-}
--  getClosureDict = (static (\Dict -> Dict)) `cap` getClosureDict

--instance (Typeable a, Typeable b, Static (Binary a), Static (Binary b)) => Static (Binary (a,b)) where
--  getClosureDict = (static (\Dict Dict -> Dict)) `cap` (getClosureDict :: Closure t => t (Dict (Binary a))) `cap` (getClosureDict :: Closure t => t (Dict (Binary b)))

-- $(getAllInstances ''Binary)
$(mkAllInstances 'getClosureDict ''HasClosureDict ''Binary)
