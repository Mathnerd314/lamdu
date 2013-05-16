{-# LANGUAGE TemplateHaskell #-}
module Lamdu.Data.Expression.Infer.Rules
  ( Rule(..)
  , makeForAll, makeForNode
  , union
  , runClosure
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.DeepSeq (NFData(..))
import Control.Lens ((^.), (^..), (&), (%~), (.~))
import Control.Monad (guard)
import Control.Monad.Trans.State (State)
import Control.Monad.Trans.Writer (execWriter)
import Control.Monad.Unit (Unit(..))
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.Derive.NFData (makeNFData)
import Data.DeriveTH (derive)
import Data.Functor.Identity (Identity(..))
import Data.Store.Guid (Guid)
import Data.Traversable (traverse, sequenceA)
import Lamdu.Data.Expression.Infer.Types
import qualified Control.Compose as Compose
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.IntSet as IntSet
import qualified Data.List.Assoc as AssocList
import qualified Data.Monoid as Monoid
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type RuleFunction def = [RefExpression def] -> [(ExprRef, RefExpression def)]

type Origin2 = (Origin, Origin)
type Origin3 = (Origin, Origin, Origin)
mkOrigin2 :: State Origin Origin2
mkOrigin2 = (,) <$> mkOrigin <*> mkOrigin
mkOrigin3 :: State Origin Origin3
mkOrigin3 = (,,) <$> mkOrigin <*> mkOrigin <*> mkOrigin

-- Boilerplate to work around lack of serialization of functions
-- Represents a serialization of RuleFunction:
data RuleClosure def
  = LambdaBodyTypeToPiResultTypeClosure (Guid, ExprRef) Origin2
  | PiToLambdaClosure (Guid, ExprRef, ExprRef) Origin3
  | RecordValToTypeClosure ([Expression.FieldTag], ExprRef) Origin
  | RecordTypeToFieldTypesClosure [(Expression.FieldTag, ExprRef)]
  | CopyClosure ExprRef
  | LambdaParentToChildrenClosure (Expression.Lambda ExprRef)
  | LambdaChildrenToParentClosure (Expression.Kind, Guid, ExprRef) Origin
  | RecordParentToChildrenClosure (Expression.Record ExprRef)
  | RecordChildrenToParentClosure (Expression.Kind, [Expression.FieldTag], ExprRef) Origin
  | SetClosure [(ExprRef, RefExpression def)]
  | SimpleTypeClosure ExprRef Origin2
  | IntoApplyResultClosure (Expression.Kind, ExprRef, ExprRef)
  | IntoArgClosure (Expression.Kind, ExprRef)
  | IntoFuncResultTypeClosure (Expression.Kind, ExprRef)
  | ArgTypeToPiParamTypeClosure ExprRef Origin2
  | RigidArgApplyTypeToResultTypeClosure ExprRef Origin3
  | RedexApplyTypeToResultTypeClosure ExprRef
  | PiParamTypeToArgTypeClosure ExprRef
  | LambdaParamTypeToArgTypeClosure ExprRef
  | ArgTypeToLambdaParamTypeClosure ExprRef Origin
  | NonLambdaToApplyValueClosure ExprRef Origin
  | ApplyToPartsClosure (Expression.Apply ExprRef)

data Rule def = Rule
  { ruleInputs :: [ExprRef]
  , _ruleCompute :: RuleClosure def
  }

fmap concat . sequence $
  derive
  <$> [makeBinary, makeNFData]
  <*> [''RuleClosure, ''Rule]

runClosure :: Eq def => RuleClosure def -> RuleFunction def
runClosure closure =
  case closure of
  LambdaBodyTypeToPiResultTypeClosure x o ->
    runLambdaBodyTypeToPiResultTypeClosure x o
  PiToLambdaClosure x o ->
    runPiToLambdaClosure x o
  RecordValToTypeClosure x o ->
    runRecordValToTypeClosure x o
  RecordTypeToFieldTypesClosure x ->
    runRecordTypeToFieldTypesClosure x
  CopyClosure x ->
    runCopyClosure x
  LambdaParentToChildrenClosure x ->
    runLambdaParentToChildrenClosure x
  LambdaChildrenToParentClosure x o ->
    runLambdaChildrenToParentClosure x o
  RecordParentToChildrenClosure x ->
    runRecordParentToChildrenClosure x
  RecordChildrenToParentClosure x o ->
    runRecordChildrenToParentClosure x o
  SetClosure x ->
    runSetClosure x
  SimpleTypeClosure x o ->
    runSimpleTypeClosure x o
  IntoApplyResultClosure x ->
    runIntoApplyResultClosure x
  IntoArgClosure x ->
    runIntoArgClosure x
  IntoFuncResultTypeClosure x ->
    runIntoFuncResultTypeClosure x
  ArgTypeToPiParamTypeClosure x o ->
    runArgTypeToPiParamTypeClosure x o
  RigidArgApplyTypeToResultTypeClosure x o ->
    runRigidArgApplyTypeToResultTypeClosure x o
  RedexApplyTypeToResultTypeClosure x ->
    runRedexApplyTypeToResultTypeClosure x
  PiParamTypeToArgTypeClosure x ->
    runPiParamTypeToArgTypeClosure x
  LambdaParamTypeToArgTypeClosure x ->
    runLambdaParamTypeToArgTypeClosure x
  ArgTypeToLambdaParamTypeClosure x o ->
    runArgTypeToLambdaParamTypeClosure x o
  NonLambdaToApplyValueClosure x o ->
    runNonLambdaToApplyValueClosure x o
  ApplyToPartsClosure x ->
    runApplyToPartsClosure x

makeForNode :: Expression.Expression def TypedValue -> State Origin [Rule def]
makeForNode (Expression.Expression exprBody typedVal) =
  (:)
  <$> ruleSimpleType typedVal
  <*>
  case Lens.view Expression.ePayload <$> exprBody of
  Expression.BodyLam lambda ->
    (++) <$> lamKindRules lambda <*> onLambda lambda
  Expression.BodyApply apply -> applyRules typedVal apply
  Expression.BodyRecord record ->
    (++)
    <$> recordKindRules record
    <*> recordStructureRules (tvVal typedVal) (fmap tvVal record)
  Expression.BodyGetField getField ->
    -- TODO: GetField Rules
    pure []
  -- Leafs need no additional rules beyond the commonal simpleTypeRule
  Expression.BodyLeaf _ -> pure []
  where
    recordKindRules (Expression.Record Expression.Type fields) =
      mapM (setRule . tvType . snd) fields
    recordKindRules (Expression.Record Expression.Val fields) =
      recordValueRules (tvType typedVal) $ fields & Lens.mapped . Lens._2 %~ tvType
    lamKindRules (Expression.Lambda Expression.Type _ _ body) =
      fmap (:[]) . setRule $ tvType body
    lamKindRules (Expression.Lambda Expression.Val param _ body) =
      lambdaRules param typedVal (tvType body)
    onLambda lam =
      (:) <$> setRule (tvType (lam ^. Expression.lambdaParamType)) <*>
      lambdaStructureRules (tvVal typedVal) (fmap tvVal lam)
    setRule ref = do
      o <- mkOrigin
      return . Rule [] $ SetClosure [(ref, setExpr o)]

makeForAll :: Expression.Expression def TypedValue -> State Origin [Rule def]
makeForAll = fmap concat . traverse makeForNode . ExprUtil.subExpressions

makeHole :: Origin -> RefExpression def
makeHole g = makeRefExpr g $ Expression.BodyLeaf Expression.Hole

setExpr :: Origin -> RefExpression def
setExpr g = makeRefExpr g $ Expression.BodyLeaf Expression.Set

intTypeExpr :: Origin -> RefExpression def
intTypeExpr g = makeRefExpr g $ Expression.BodyLeaf Expression.IntegerType

guidFromOrigin :: Origin -> Guid
guidFromOrigin origin = Guid.fromString $ show origin ++ "(orig)"

makePi :: Origin -> RefExpression def -> RefExpression def -> RefExpression def
makePi o paramType result =
  makeRefExpr o $ ExprUtil.makePi (guidFromOrigin o) paramType result

runLambdaBodyTypeToPiResultTypeClosure :: (Guid, ExprRef) -> Origin2 -> RuleFunction def
runLambdaBodyTypeToPiResultTypeClosure (param, lambdaTypeRef) (o0, o1) ~[bodyTypeExpr] =
  [( lambdaTypeRef
   , makeRefExpr o0 $ ExprUtil.makePi param (makeHole o1) bodyTypeExpr
   )]

runPiToLambdaClosure :: (Guid, ExprRef, ExprRef) -> Origin3 -> RuleFunction def
runPiToLambdaClosure (param, lambdaValueRef, bodyTypeRef) (o0, o1, o2) ~[piBody] = do
  Expression.Lambda Expression.Type piParam paramType resultType <-
    piBody ^.. Expression.eBody . Expression._BodyLam
  [ -- Pi result type -> Body type
    ( bodyTypeRef
    , subst piParam
      ( makeRefExpr o0
        (Lens.review ExprUtil.bodyParameterRef param)
      )
      resultType
    )
    , -- Pi param type -> Lambda param type
      ( lambdaValueRef
      , makeRefExpr o1 . ExprUtil.makeLambda piParam paramType $ makeHole o2
      )
    ]

lambdaRules :: Guid -> TypedValue -> ExprRef -> State Origin [Rule def]
lambdaRules param (TypedValue lambdaValueRef lambdaTypeRef) bodyTypeRef =
  sequenceA
  [ Rule [bodyTypeRef] . LambdaBodyTypeToPiResultTypeClosure (param, lambdaTypeRef) <$> mkOrigin2
  , Rule [lambdaTypeRef] . PiToLambdaClosure (param, lambdaValueRef, bodyTypeRef) <$> mkOrigin3
  ]

runRecordValToTypeClosure :: ([Expression.FieldTag], ExprRef) -> Origin -> RuleFunction def
runRecordValToTypeClosure (fields, recordTypeRef) o0 fieldTypeExprs =
  [ ( recordTypeRef
    , makeRefExpr o0 . Expression.BodyRecord . Expression.Record Expression.Type $
      zip fields fieldTypeExprs
    )
  ]

runRecordTypeToFieldTypesClosure :: [(Expression.FieldTag, ExprRef)] -> RuleFunction def
runRecordTypeToFieldTypesClosure fieldTypeRefs ~[recordTypeExpr] = do
  Expression.Record _ fieldTypeExprs <-
    recordTypeExpr ^.. Expression.eBody . Expression._BodyRecord
  maybe [] (map snd) $ AssocList.match (,) fieldTypeRefs fieldTypeExprs

recordValueRules :: ExprRef -> [(Expression.FieldTag, ExprRef)] -> State Origin [Rule def]
recordValueRules recTypeRef fieldTypeRefs =
  sequenceA
  [ Rule typeRefs . RecordValToTypeClosure (fields, recTypeRef) <$> mkOrigin
  , pure . Rule [recTypeRef] $ RecordTypeToFieldTypesClosure fieldTypeRefs
  ]
  where
    (fields, typeRefs) = unzip fieldTypeRefs

runCopyClosure :: ExprRef -> RuleFunction def
runCopyClosure dest ~[srcExpr] = [(dest, srcExpr)]

union :: ExprRef -> ExprRef -> [Rule def]
union x y =
  [ Rule [x] $ CopyClosure y
  , Rule [y] $ CopyClosure x
  ]

-- Parent lambda to children
runLambdaParentToChildrenClosure :: Expression.Lambda ExprRef -> RuleFunction def
runLambdaParentToChildrenClosure (Expression.Lambda _ _ paramTypeRef resultRef) ~[expr] = do
  Expression.Lambda _ _ paramTypeE resultE <-
    expr ^.. Expression.eBody . Expression._BodyLam
  [(paramTypeRef, paramTypeE), (resultRef, resultE)]

-- Children of lambda to lambda parent
runLambdaChildrenToParentClosure :: (Expression.Kind, Guid, ExprRef) -> Origin -> RuleFunction def
runLambdaChildrenToParentClosure (k, param, lamRef) o0 ~[paramTypeExpr, resultExpr] =
  [( lamRef
   , makeRefExpr o0 . Expression.BodyLam $
     Expression.Lambda k param paramTypeExpr resultExpr
   )]

lambdaStructureRules :: ExprRef -> Expression.Lambda ExprRef -> State Origin [Rule def]
lambdaStructureRules lamRef lam@(Expression.Lambda k param paramTypeRef resultRef) =
  sequenceA
  [ pure . Rule [lamRef] $ LambdaParentToChildrenClosure lam
  , -- Copy the structure from the children to the parent
    Rule [paramTypeRef, resultRef] .
    LambdaChildrenToParentClosure (k, param, lamRef) <$> mkOrigin
  ]

runRecordParentToChildrenClosure :: Expression.Record ExprRef -> RuleFunction def
runRecordParentToChildrenClosure (Expression.Record _ fieldRefs) ~[expr] = do
  Expression.Record _ fieldExprs <-
    expr ^.. Expression.eBody . Expression._BodyRecord
  maybe [] (map snd) $ AssocList.match (,) fieldRefs fieldExprs

runRecordChildrenToParentClosure ::
  (Expression.Kind, [Expression.FieldTag], ExprRef) -> Origin -> RuleFunction def
runRecordChildrenToParentClosure (k, fields, recRef) o0 fieldExprs =
  [( recRef
   , makeRefExpr o0 . Expression.BodyRecord .
     Expression.Record k $ zip fields fieldExprs
   )]

recordStructureRules :: ExprRef -> Expression.Record ExprRef -> State Origin [Rule def]
recordStructureRules recRef rec@(Expression.Record k fields) =
  sequenceA
  [ pure . Rule [recRef] $ RecordParentToChildrenClosure rec
  , Rule valRefs .
    RecordChildrenToParentClosure (k, keys, recRef) <$> mkOrigin
  ]
  where
    (keys, valRefs) = unzip fields

runSetClosure :: [(ExprRef, RefExpression def)] -> RuleFunction def
runSetClosure outputs ~[] = outputs

subst ::
  Guid -> Expression.Expression def a ->
  Expression.Expression def a -> Expression.Expression def a
subst from to expr
  | Lens.anyOf
    (Expression.eBody . ExprUtil.bodyParameterRef)
    (== from) expr
  = to
  | otherwise = expr & Expression.eBody . Lens.traversed %~ subst from to

mergeToPiResult ::
  Eq def => RefExpression def -> RefExpression def -> RefExpression def
mergeToPiResult =
  fmap runIdentity .
  ExprUtil.matchExpression onMatch ((fmap . fmap) return onMismatch)
  where
    onMatch x _ = return x
    onMismatch dest src
      -- TODO: This seems like it should report an error,
      -- verify/document that it is OK because the other direction of
      -- information flow will catch any error:
      | notAHole dest = dest
      | not (IntSet.null substs) =
        Lens.set rplSubstitutedArgs substs <$> src
      | notAHole src =
        Lens.set rplRestrictedPoly (Monoid.Any True) <$> dest
      | otherwise = dest
      where
        substs = dest ^. Expression.ePayload . rplSubstitutedArgs
    notAHole = Lens.nullOf (Expression.eBody . Expression._BodyLeaf . Expression._Hole)

runSimpleTypeClosure :: ExprRef -> Origin2 -> RuleFunction def
runSimpleTypeClosure typ (o0, o1) ~[valExpr] =
  case valExpr ^. Expression.eBody of
  Expression.BodyLeaf Expression.Set -> simpleType
  Expression.BodyLeaf Expression.IntegerType -> simpleType
  Expression.BodyLam (Expression.Lambda Expression.Type _ _ _) -> simpleType
  Expression.BodyRecord (Expression.Record Expression.Type _) -> simpleType
  Expression.BodyRecord (Expression.Record Expression.Val _) ->
    -- The rule to copy inferred types of fields to the inferred type
    -- of the whole record requiers dereferencing the inferred types
    -- of the field exprs which is impossible in this context. This is
    -- handled in the recordValueRules
    []
  Expression.BodyLeaf (Expression.LiteralInteger _) -> [(typ, intTypeExpr o0)]
  Expression.BodyLam
    (Expression.Lambda Expression.Val param paramType _) ->
    [( typ
     , makeRefExpr o0 . ExprUtil.makePi param paramType $
       makeHole o1
     )]
  -- All type information that can be deduced from these depends on
  -- external information which is not used in the simple type rules.
  Expression.BodyLeaf Expression.GetVariable {} -> []
  Expression.BodyLeaf Expression.Hole {} -> []
  Expression.BodyApply {} -> []
  Expression.BodyGetField {} -> []
  where
    simpleType = [(typ, setExpr o0)]

ruleSimpleType :: TypedValue -> State Origin (Rule def)
ruleSimpleType (TypedValue val typ) = Rule [val] . SimpleTypeClosure typ <$> mkOrigin2

runIntoApplyResultClosure :: (Expression.Kind, ExprRef, ExprRef) -> RuleFunction def
runIntoApplyResultClosure (k, applyRef, arg) ~[funcExpr, argExpr] = do
  Expression.Lambda bk param _ result <-
    funcExpr ^.. Expression.eBody . Expression._BodyLam
  guard $ k == bk
  return
    ( applyRef
    , subst param
      (argExpr & Lens.traversed . rplSubstitutedArgs %~ IntSet.insert (unExprRef arg)) .
      -- TODO: Is this correct?
      Lens.set (Lens.traversed . rplRestrictedPoly) (Monoid.Any False) $
      result
    )

intoApplyResultRule :: Expression.Kind -> ExprRef -> ExprRef -> ExprRef -> Rule def
intoApplyResultRule k applyRef func arg =
  -- PreSubst with Subst => PostSubst
  -- (func, arg) -> apply
  Rule [func, arg] $ IntoApplyResultClosure (k, applyRef, arg)

runIntoArgClosure :: Eq def => (Expression.Kind, ExprRef) -> RuleFunction def
runIntoArgClosure (k, arg) ~[applyExpr, funcExpr] = do
  -- Recurse over PreSubst and PostSubst together
  --   When PreSubst part refers to its param:
  --     PostSubst part <=> arg
  -- (apply, func) -> arg
  Expression.Lambda bk param _ result <-
    funcExpr ^.. Expression.eBody . Expression._BodyLam
  guard $ bk == k
  mergeToArg param arg result applyExpr

intoArgRule :: Expression.Kind -> ExprRef -> ExprRef -> ExprRef -> Rule def
intoArgRule k applyRef func arg =
  Rule [applyRef, func] $ IntoArgClosure (k, arg)

runIntoFuncResultTypeClosure :: Eq def => (Expression.Kind, ExprRef) -> RuleFunction def
runIntoFuncResultTypeClosure (k, func) ~[applyExpr, Expression.Expression funcBody funcPl] = do
  Expression.Lambda kb param paramT result <- funcBody ^.. Expression._BodyLam
  guard $ k == kb
  return
    ( func
    , makeRefExpr (Lens.view rplOrigin funcPl) .
      Expression.BodyLam . Expression.Lambda k param paramT $
      mergeToPiResult result applyExpr
    )

intoFuncResultTypeRule :: Expression.Kind -> ExprRef -> ExprRef -> Rule def
intoFuncResultTypeRule k applyRef func =
  -- Propagate data from Apply's to the Func where appropriate.
  -- (Not on non-substituted holes)
  -- apply -> func result
  Rule [applyRef, func] $ IntoFuncResultTypeClosure (k, func)

recurseSubstRules :: Expression.Kind -> ExprRef -> ExprRef -> ExprRef -> [Rule def]
recurseSubstRules k applyRef func arg =
  [ intoApplyResultRule k applyRef func arg
  , intoArgRule k applyRef func arg
  , intoFuncResultTypeRule k applyRef func
  ]

-- param, (dest)argRef, func result, applyExpr
mergeToArg :: Eq def => Guid -> ExprRef -> RefExpression def -> RefExpression def -> [(ExprRef, RefExpression def)]
mergeToArg param arg =
  (fmap . fmap) (execWriter . Compose.unO) $
  ExprUtil.matchExpression onMatch onMismatch
  where
    unit = Compose.O (pure Unit)
    onMatch _ _ = unit
    onMismatch expr post
      | Lens.anyOf
        (Expression.eBody . ExprUtil.bodyParameterRef)
        (== param) expr
      = Compose.O . (fmap . const) Unit $ Writer.tell
        [( arg
         , post
           & Lens.traversed . rplSubstitutedArgs %~ IntSet.delete (unExprRef arg)
           -- TODO: Is this correct?
           & Lens.traversed . rplRestrictedPoly .~ Monoid.Any False
         )]
      | otherwise = unit

runArgTypeToPiParamTypeClosure :: ExprRef -> Origin2 -> RuleFunction def
runArgTypeToPiParamTypeClosure funcTypeRef (o0, o1) ~[argTypeExpr] =
  [( funcTypeRef
   , makePi o0 argTypeExpr $ makeHole o1
   )]

argTypeToPiParamTypeRule :: Expression.Apply TypedValue -> State Origin (Rule def)
argTypeToPiParamTypeRule (Expression.Apply func arg) =
  -- ArgT => Pi ParamT
  Rule [tvType arg] . ArgTypeToPiParamTypeClosure (tvType func) <$> mkOrigin2

runRigidArgApplyTypeToResultTypeClosure :: ExprRef -> Origin3 -> RuleFunction def
runRigidArgApplyTypeToResultTypeClosure funcTypeRef (o0, o1, o2) ~[applyTypeExpr, argExpr] = do
  par <-
    argExpr ^..
    Expression.eBody . Expression._BodyLeaf . Expression._GetVariable . Expression._ParameterRef
  return
    ( funcTypeRef
    , makePi o0 (makeHole o1) $
      subst par (makeHole o2) applyTypeExpr
    )

rigidArgApplyTypeToResultTypeRule ::
  TypedValue -> Expression.Apply TypedValue -> State Origin (Rule def)
rigidArgApplyTypeToResultTypeRule applyTv (Expression.Apply func arg) =
  -- If Arg is GetParam
  -- ApplyT (Susbt Arg with Hole) => ResultT
  Rule [tvType applyTv, tvVal arg] .
  RigidArgApplyTypeToResultTypeClosure (tvType func) <$> mkOrigin3

runRedexApplyTypeToResultTypeClosure :: ExprRef -> RuleFunction def
runRedexApplyTypeToResultTypeClosure funcTypeRef
  ~[applyTypeExpr, Expression.Expression funcExpr funcPl] = do
  Expression.Lambda Expression.Val paramGuid paramType _ <-
    funcExpr ^.. Expression._BodyLam
  return
    ( funcTypeRef
    , makeRefExpr (Lens.view rplOrigin funcPl) $
      ExprUtil.makePi paramGuid paramType applyTypeExpr
    )

redexApplyTypeToResultTypeRule :: TypedValue -> TypedValue -> Rule def
redexApplyTypeToResultTypeRule applyTv funcTv =
  Rule [tvType applyTv, tvVal funcTv] $
  RedexApplyTypeToResultTypeClosure (tvType funcTv)

runPiParamTypeToArgTypeClosure :: ExprRef -> RuleFunction def
runPiParamTypeToArgTypeClosure argTypeRef ~[Expression.Expression funcTExpr _] = do
  -- If func type is Pi
  -- Pi's ParamT => ArgT
  Expression.Lambda Expression.Type _ paramT _ <- funcTExpr ^.. Expression._BodyLam
  return (argTypeRef, paramT)

piParamTypeToArgTypeRule :: Expression.Apply TypedValue -> Rule def
piParamTypeToArgTypeRule (Expression.Apply func arg) =
  Rule [tvType func] $ PiParamTypeToArgTypeClosure (tvType arg)

runLambdaParamTypeToArgTypeClosure :: ExprRef -> RuleFunction def
runLambdaParamTypeToArgTypeClosure argTypeRef ~[Expression.Expression funcExpr _] = do
  -- If func is Lambda
  -- Lambda's ParamT => ArgT
  Expression.Lambda Expression.Val _ paramT _ <-
    funcExpr ^.. Expression._BodyLam
  return (argTypeRef, paramT)

lambdaParamTypeToArgTypeRule :: Expression.Apply TypedValue -> Rule def
lambdaParamTypeToArgTypeRule (Expression.Apply func arg) =
  Rule [tvVal func] $ LambdaParamTypeToArgTypeClosure (tvType arg)

runArgTypeToLambdaParamTypeClosure :: ExprRef -> Origin -> RuleFunction def
runArgTypeToLambdaParamTypeClosure funcValRef o0 ~[Expression.Expression funcExpr funcPl, argTExpr] = do
  -- If func is Lambda,
  -- ArgT => Lambda's ParamT
  Expression.Lambda Expression.Val param _ _ <-
    funcExpr ^.. Expression._BodyLam
  return
    ( funcValRef
    , makeRefExpr (Lens.view rplOrigin funcPl) .
      ExprUtil.makeLambda param argTExpr $ makeHole o0
    )

argTypeToLambdaParamTypeRule :: Expression.Apply TypedValue -> State Origin (Rule def)
argTypeToLambdaParamTypeRule (Expression.Apply func arg) =
  Rule [tvVal func, tvType arg] . ArgTypeToLambdaParamTypeClosure (tvVal func) <$> mkOrigin

runNonLambdaToApplyValueClosure :: ExprRef -> Origin -> RuleFunction def
runNonLambdaToApplyValueClosure applyValRef o0 ~[funcExpr, argExpr] =
  -- If func is surely not a lambda (a hole too could be a lambda).
  --
  -- Applies have a special case in the inferred value handling for
  -- redexes, and this rule is about detecting that an application is
  -- *not* a redex, so we can safely set the inferred value to the
  -- application itself.
  --
  -- Func Arg => Outer
  case funcExpr ^. Expression.eBody of
  Expression.BodyLam (Expression.Lambda Expression.Val _ _ _) -> []
  Expression.BodyLeaf Expression.Hole -> []
  _ ->
    [ ( applyValRef
      , makeRefExpr o0 $ ExprUtil.makeApply funcExpr argExpr
      )
    ]

nonLambdaToApplyValueRule :: TypedValue -> Expression.Apply TypedValue -> State Origin (Rule def)
nonLambdaToApplyValueRule applyTv (Expression.Apply func arg) =
  Rule [tvVal func, tvVal arg] .
  NonLambdaToApplyValueClosure (tvVal applyTv) <$> mkOrigin

runApplyToPartsClosure :: Eq def => Expression.Apply ExprRef -> RuleFunction def
runApplyToPartsClosure refs ~[applyExpr, funcExpr] = do
  -- If definitely not a redex (func also not a hole)
  -- Apply-Arg => Arg
  -- Apply-Func => Func
  guard $ Lens.nullOf
    (Expression.eBody . Expression._BodyLam . Expression.lambdaKind . Expression._Val)
    funcExpr
  guard $ Lens.nullOf (Expression.eBody . Expression._BodyLeaf . Expression._Hole) funcExpr
  Expression.Apply aFunc aArg <- applyExpr ^.. Expression.eBody . Expression._BodyApply
  [(refs ^. Expression.applyFunc, aFunc), (refs ^. Expression.applyArg, aArg)]

applyToPartsRule ::
  TypedValue -> Expression.Apply TypedValue -> Rule def
applyToPartsRule applyTv parts@(Expression.Apply func _) =
  Rule [tvVal applyTv, tvVal func] . ApplyToPartsClosure $ tvVal <$> parts

applyRules :: TypedValue -> Expression.Apply TypedValue -> State Origin [Rule def]
applyRules applyTv apply@(Expression.Apply func arg) =
  -- TODO: make all of these functions have a standard signature and
  -- just apply them all to the same args?
  (++ pureRules) <$>
  sequenceA
  [ argTypeToPiParamTypeRule apply
  , rigidArgApplyTypeToResultTypeRule applyTv apply
  , argTypeToLambdaParamTypeRule apply
  , nonLambdaToApplyValueRule applyTv apply
  ]
  where
    pureRules =
      [ piParamTypeToArgTypeRule apply
      , redexApplyTypeToResultTypeRule applyTv func
      , lambdaParamTypeToArgTypeRule apply
      , applyToPartsRule applyTv apply
      ]
      ++ recurseSubstRules Expression.Type
        (tvType applyTv) (tvType func) (tvVal arg)
      ++ recurseSubstRules Expression.Val
        (tvVal applyTv) (tvVal func) (tvVal arg)
