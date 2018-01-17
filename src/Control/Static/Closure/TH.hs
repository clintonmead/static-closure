{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StaticPointers #-}

module Control.Static.Closure.TH where

import Data.Monoid (Endo(Endo, appEndo), (<>), mempty)
import Data.Typeable (Typeable)
import Control.Static.Closure.IsClosure (IsClosure(cap))
import Data.Constraint (Dict(Dict))

import Language.Haskell.TH (
  Name,
  Q,
  Info(ClassI),
  InstanceDec,
  Dec(InstanceD, FunD),
  Type(ForallT, AppT, SigT, VarT, ConT, InfixT, UInfixT, ParensT),
  Exp(VarE, ConE, InfixE, LamE, SigE, StaticE),
  Clause(Clause),
  Body(NormalB),
  Pat(ConP),
  reify,
  mkName
  )

getAllInstances :: Name -> Q [InstanceDec]
getAllInstances className = do
  result <- reify className
  case result of
    ClassI _ instanceDecs -> pure instanceDecs
    _ -> fail "getAllInstances: Not a class"

mkInstance :: Name -> Name -> InstanceDec -> InstanceDec
mkInstance getClosureDictName staticClassName instanceDec = case instanceDec of
  InstanceD maybeOverlap oldCxt oldType _ ->
    let
      dictTypeName = ''Dict
      dictType = ConT dictTypeName
      dictValueName = 'Dict
      dictValue = ConE dictValueName
      dictPat = ConP dictValueName []
      dummyTypeName = mkName "t"
      dummyType = VarT dummyTypeName
      closureClassName = ''IsClosure
      closureClass = ConT closureClassName
      capName = 'cap
      capValue = VarE capName
      getClosureDict = VarE getClosureDictName
      addClassF = AppT (ConT staticClassName)
      addTypeableF = AppT (ConT ''Typeable)
      newType = addClassF oldType
      newStaticCxt = addClassF <$> oldCxt
      newTypeableCxt = (addTypeableF . VarT) <$> ((oldType:oldCxt) >>= findAllTypeVars)
      newCxt = newTypeableCxt ++ newStaticCxt
      mkTypeSig cxt = ForallT [] [AppT closureClass dummyType] (AppT dummyType (AppT dictType cxt))
      mkArgExp cxt = SigE getClosureDict (mkTypeSig cxt)
      addArg x cxt = InfixE (Just x) capValue (Just (mkArgExp cxt))
      funcPart = case (length oldCxt) of
        0 -> dictValue
        n -> LamE (replicate n dictPat) dictValue
      body = NormalB (foldl addArg (StaticE funcPart) oldCxt)
      clause = Clause [] body []
      funClause = FunD getClosureDictName [clause]
    in
      InstanceD maybeOverlap newCxt newType [funClause]
  _ -> error "mkInstance: Not an instance"

mkAllInstances :: Name -> Name -> Name -> Q [InstanceDec]
mkAllInstances getClosureDictName staticClassName className = (fmap . fmap) (mkInstance getClosureDictName staticClassName) (getAllInstances className)

findAllTypeVars :: Type -> [Name]
findAllTypeVars x = appEndo (go x) [] where
  go = \case
    ForallT{} -> error "Don't know how do deal with Foralls in types"
    AppT t1 t2 -> go t1 <> go t2
    SigT t _ -> go t
    VarT name -> Endo (name:)
    InfixT t1 _ t2 -> go t1 <> go t2
    UInfixT t1 _ t2 -> go t1 <> go t2
    ParensT t -> go t
    _ -> mempty
