{-# OPTIONS -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, RankNTypes, NoMonomorphismRestriction #-}

module Lamdu.Data.Expression.Utils
  ( makeApply
  , makePi, makeLambda, makeLam
  , pureApply
  , pureHole
  , pureSet
  , pureRecord
  , pureLam
  , pureGetField
  , pureLiteralInteger
  , pureIntegerType
  , pureExpression
  , randomizeExpr
  , canonizeParamIds, randomizeParamIds
  , randomizeParamIdsG, NameGen(..), randomNameGen, debugNameGen
  , matchBody, matchExpression
  , subExpressions, subExpressionsWithoutTags
  , isDependentPi
  , curriedFuncArguments
  , applyForms, applyDependentPis
  , alphaEq
  , subst, substGetPar
  , subExpressionsThat
  , showBodyExpr, showsPrecBodyExpr
  ) where

import Prelude hiding (pi)
import Lamdu.Data.Expression

import Control.Applicative (Applicative(..), liftA2, (<$>))
import Control.Lens.Operators
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad.Trans.State (evalState, state)
import Data.Map (Map)
import Data.Maybe (isJust, fromMaybe)
import Data.Monoid (Any)
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable(..), sequenceA)
import System.Random (Random, RandomGen, random)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.List.Utils as ListUtils
import qualified Data.Map as Map
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified System.Random as Random

data PiWrappers def a = PiWrappers
  { _dependentPiParams :: [(Guid, Expression def a)]
  , nonDependentPiParams :: [(Guid, Expression def a)]
  }
Lens.makeLenses ''PiWrappers

getPiWrappers :: Expression def a -> PiWrappers def a
getPiWrappers expr =
  case expr ^? ExprLens.exprLam of
  Just (Lambda Type param paramType resultType)
    | isDependentPi expr ->
      getPiWrappers resultType & dependentPiParams %~ addParam
    | otherwise ->
        PiWrappers
        { _dependentPiParams = []
        , nonDependentPiParams = addParam (getParams resultType)
        }
    where
      addParam = ((param, paramType) :)
  _ -> PiWrappers [] []

getDependentParams :: Expression def a -> [(Guid, Expression def a)]
getDependentParams = (^. dependentPiParams) . getPiWrappers

compose :: [a -> a] -> a -> a
compose = foldr (.) id
{-# INLINE compose #-}

applyWith :: [Expression def ()] -> Expression def () -> Expression def ()
applyWith =
  compose . map addApply
  where
    addApply arg = pureExpression . (`makeApply` arg)

applyWithHoles :: Int -> Expression def () -> Expression def ()
applyWithHoles count = applyWith $ replicate count pureHole

applyDependentPis :: Expression def () -> Expression def () -> Expression def ()
applyDependentPis exprType = applyWithHoles (length (getDependentParams exprType))

alphaEq :: Eq def => Expression def a -> Expression def a -> Bool
alphaEq x y =
  isJust $ matchExpression
  ((const . const . Just) ())
  ((const . const) Nothing)
  x y

-- Useful functions:
substGetPar ::
  Guid ->
  Expression def a ->
  Expression def a ->
  Expression def a
substGetPar from =
  subst (ExprLens.exprParameterRef . Lens.filtered (== from))

subst ::
  (Lens.Getting Any (Expression def a) b) ->
  Expression def a ->
  Expression def a ->
  Expression def a
subst lens to expr
  | Lens.has lens expr = to
  | otherwise = expr & eBody . traverse %~ subst lens to

-- Transform expression to expression applied with holes,
-- with all different sensible levels of currying.
applyForms :: Expression def () -> Expression def () -> [Expression def ()]
applyForms exprType expr
  | Lens.notNullOf (ExprLens.exprLam . lambdaKind . _Val) expr = [expr]
  | otherwise = reverse $ scanl (flip addApply) withDepPisApplied nonDepParams
  where
    withDepPisApplied = applyWithHoles (length depParams) expr
    PiWrappers
      { _dependentPiParams = depParams
      , nonDependentPiParams = nonDepParams
      } = getPiWrappers exprType
    addApply (_, paramType) =
      pureExpression . (`makeApply` arg)
      where
        arg =
          case paramType ^? ExprLens.exprKindedRecordFields Type of
          Just fields ->
            ExprLens.pureExpr . _BodyRecord . ExprLens.kindedRecordFields Val #
            (fields & Lens.mapped . Lens._2 .~ pureHole)
          _ -> pureHole
randomizeExpr :: (RandomGen g, Random r) => g -> Expression def (r -> a) -> Expression def a
randomizeExpr gen = (`evalState` gen) . traverse randomize
  where
    randomize f = f <$> state random

data NameGen = NameGen
  { ngSplit :: (NameGen, NameGen)
  , ngNext :: (Guid, NameGen)
  }

randomNameGen :: RandomGen g => g -> NameGen
randomNameGen g = NameGen
  { ngSplit = Random.split g & Lens.both %~ randomNameGen
  , ngNext = random g & Lens._2 %~ randomNameGen
  }

debugNameGen :: NameGen
debugNameGen = ng names ""
  where
    names = (:[]) <$> ['a'..'z']
    ng [] _ = error "TODO: Infinite list of names"
    ng st@(l:ls) suffix =
      NameGen
      { ngSplit = (ng st "_0", ng st "_1")
      , ngNext = (Guid.fromString (l++suffix), ng ls suffix)
      }

canonizeParamIds :: Expression def a -> Expression def a
canonizeParamIds = randomizeParamIds (Random.mkStdGen 0)

randomizeParamIds :: RandomGen g => g -> Expression def a -> Expression def a
randomizeParamIds gen = randomizeParamIdsG (randomNameGen gen) Map.empty $ \_ _ a -> a

randomizeParamIdsG ::
  NameGen -> Map Guid Guid ->
  (NameGen -> Map Guid Guid -> a -> b) ->
  Expression def a -> Expression def b
randomizeParamIdsG gen initMap convertPL =
  (`evalState` gen) . (`runReaderT` initMap) . go
  where
    go (Expression v s) = do
      guidMap <- Reader.ask
      newGen <- lift $ state ngSplit
      (`Expression` convertPL newGen guidMap s) <$>
        case v of
        BodyLam (Lambda k oldParamId paramType body) -> do
          newParamId <- lift $ state ngNext
          fmap BodyLam $ liftA2 (Lambda k newParamId) (go paramType) .
            Reader.local (Map.insert oldParamId newParamId) $ go body
        BodyLeaf (GetVariable (ParameterRef guid)) ->
          pure $ ExprLens.bodyParameterRef #
          fromMaybe guid (Map.lookup guid guidMap)
        x@BodyLeaf {}     -> traverse go x
        x@BodyApply {}    -> traverse go x
        x@BodyGetField {} -> traverse go x
        x@BodyRecord {}   -> traverse go x

-- Left-biased on parameter guids
{-# INLINE matchBody #-}
matchBody ::
  Eq def =>
  (Guid -> Guid -> a -> b -> c) -> -- ^ Lambda/Pi result match
  (a -> b -> c) ->                 -- ^ Ordinary structural match (Apply components, param type)
  (Guid -> Guid -> Bool) ->        -- ^ Match ParameterRef's
  Body def a -> Body def b -> Maybe (Body def c)
matchBody matchLamResult matchOther matchGetPar body0 body1 =
  case body0 of
  BodyLam (Lambda k0 p0 pt0 r0) -> do
    Lambda k1 p1 pt1 r1 <- body1 ^? _BodyLam
    guard $ k0 == k1
    return . BodyLam $
      Lambda k0 p0 (matchOther pt0 pt1) $
      matchLamResult p0 p1 r0 r1
  BodyApply (Apply f0 a0) -> do
    Apply f1 a1 <- body1 ^? _BodyApply
    return . BodyApply $ Apply (matchOther f0 f1) (matchOther a0 a1)
  BodyRecord (Record k0 fs0) -> do
    Record k1 fs1 <- body1 ^? _BodyRecord
    guard $ k0 == k1
    BodyRecord . Record k0 <$> ListUtils.match matchPair fs0 fs1
  BodyGetField (GetField r0 f0) -> do
    GetField r1 f1 <- body1 ^? _BodyGetField
    return . BodyGetField $ GetField (matchOther r0 r1) (matchOther f0 f1)
  BodyLeaf (GetVariable (ParameterRef p0)) -> do
    p1 <- body1 ^? ExprLens.bodyParameterRef
    guard $ matchGetPar p0 p1
    return $ Lens.review ExprLens.bodyParameterRef p0
  BodyLeaf x -> do
    y <- body1 ^? _BodyLeaf
    guard $ x == y
    return $ BodyLeaf x
  where
    matchPair (k0, v0) (k1, v1) =
      (matchOther k0 k1, matchOther v0 v1)

-- TODO: Generalize to defa/defb/defc with hof's to handle matching
-- them?  The returned expression gets the same guids as the left
-- expression
{-# INLINE matchExpression #-}
matchExpression ::
  (Eq def, Applicative f) =>
  (a -> b -> f c) ->
  (Expression def a -> Expression def b -> f (Expression def c)) ->
  Expression def a -> Expression def b -> f (Expression def c)
matchExpression onMatch onMismatch =
  go Map.empty
  where
    go scope e0@(Expression body0 pl0) e1@(Expression body1 pl1) =
      case matchBody matchLamResult matchOther matchGetPar body0 body1 of
      Nothing ->
        onMismatch e0 $
        (ExprLens.exprLeaves . ExprLens.parameterRef %~ lookupGuid) e1
      Just bodyMatched -> Expression <$> sequenceA bodyMatched <*> onMatch pl0 pl1
      where
        matchGetPar p0 p1 = p0 == lookupGuid p1
        matchLamResult p0 p1 = go $ Map.insert p1 p0 scope
        matchOther = go scope
        lookupGuid guid = fromMaybe guid $ Map.lookup guid scope

subExpressions :: Expression def a -> [Expression def a]
subExpressions x =
  x : Foldable.concatMap subExpressions (x ^. eBody)

subExpressionsWithoutTags :: Expression def a -> [Expression def a]
subExpressionsWithoutTags x =
  x :
  case x ^. eBody of
  BodyGetField (GetField record _) -> subExpressionsWithoutTags record
  BodyRecord (Record _ fields) -> concatMap subExpressionsWithoutTags (map snd fields)
  body -> Foldable.concatMap subExpressionsWithoutTags body

isDependentPi :: Expression def a -> Bool
isDependentPi =
  Lens.has (ExprLens.exprKindedLam Type . Lens.filtered f)
  where
    f (g, _, resultType) = exprHasGetVar g resultType

parameterRefs :: Lens.Fold (Expression def a) Guid
parameterRefs = Lens.folding subExpressions . ExprLens.exprParameterRef

exprHasGetVar :: Guid -> Expression def a -> Bool
exprHasGetVar g = Lens.anyOf parameterRefs (== g)

curriedFuncArguments :: Expression def a -> [Expression def a]
curriedFuncArguments =
  (^.. ExprLens.exprLam . ExprLens.kindedLam Val . Lens.folding f)
  where
    f (_, paramType, body) = paramType : curriedFuncArguments body

getParams :: Expression def a -> [(Guid, Expression def a)]
getParams expr = do
  (param, paramType, resultType) <- expr ^.. ExprLens.exprKindedLam Type
  (param, paramType) : getParams resultType

pureIntegerType :: Expression def ()
pureIntegerType = ExprLens.pureExpr . ExprLens.bodyIntegerType # ()

pureLiteralInteger :: Integer -> Expression def ()
pureLiteralInteger = (ExprLens.pureExpr . ExprLens.bodyLiteralInteger #)

pureApply :: Expression def () -> Expression def () -> Expression def ()
pureApply f x = ExprLens.pureExpr . _BodyApply # Apply f x

pureHole :: Expression def ()
pureHole = ExprLens.pureExpr . ExprLens.bodyHole # ()

pureSet :: Expression def ()
pureSet = ExprLens.pureExpr . ExprLens.bodySet # ()

pureRecord :: Kind -> [(Expression def (), Expression def ())] -> Expression def ()
pureRecord k fields = ExprLens.pureExpr . ExprLens.bodyKindedRecordFields k # fields

pureLam :: Kind -> Guid -> Expression def () -> Expression def () -> Expression def ()
pureLam k paramGuid paramType result =
  ExprLens.pureExpr . ExprLens.bodyKindedLam k # (paramGuid, paramType, result)

pureGetField :: Expression def () -> Expression def () -> Expression def ()
pureGetField record field =
  ExprLens.pureExpr . _BodyGetField # GetField record field

-- TODO: Deprecate below here:
pureExpression :: Body def (Expression def ()) -> Expression def ()
pureExpression = (ExprLens.pureExpr #)

makeApply :: expr -> expr -> Body def expr
makeApply func arg = BodyApply $ Apply func arg

makeLam :: Kind -> Guid -> expr -> expr -> Body def expr
makeLam k argId argType resultType =
  BodyLam $ Lambda k argId argType resultType

-- TODO: Remove the kind-passing wrappers
makePi :: Guid -> expr -> expr -> Body def expr
makePi = makeLam Type

makeLambda :: Guid -> expr -> expr -> Body def expr
makeLambda = makeLam Val

subExpressionsThat ::
  (Expression def a -> Bool) ->
  Lens.Fold (Expression def a) (Expression def a)
subExpressionsThat predicate =
  Lens.folding subExpressions . Lens.filtered predicate

-- Show isntances:
showsPrecBody ::
  (Show def, Show expr) => (Guid -> expr -> Bool) ->
  Int -> Body def expr -> ShowS
showsPrecBody mayDepend prec body =
  case body of
  BodyLam (Lambda Val paramId paramType result) ->
    paren 0 $
    showChar '\\' . shows paramId . showChar ':' .
    showsPrec 11 paramType . showString "==>" .
    showsPrec 0 result
  BodyLam (Lambda Type paramId paramType resultType) ->
    paren 0 $
    paramStr . showString "->" . showsPrec 0 resultType
    where
      paramStr
        | dependent =
          showString "(" . shows paramId . showString ":" . showsPrec 11 paramType . showString ")"
        | otherwise = showsPrec 1 paramType
      dependent = mayDepend paramId resultType
  BodyApply (Apply func arg) ->
    paren 10 $
    showsPrec 10 func . showChar ' ' . showsPrec 11 arg
  BodyRecord (Record k fields) ->
    paren 11 $ showString recStr
    where
      recStr =
        concat ["Rec", recType k, "{", List.intercalate ", " (map showField fields), "}"]
      showField (field, typ) =
        unwords [showsPrec 0 field "", sep k, showsPrec 0 typ ""]
      sep Val = "="
      sep Type = ":"
      recType Val = "V"
      recType Type = "T"
  BodyGetField (GetField r tag) ->
    paren 8 $ showsPrec 8 r . showChar '.' . showsPrec 9 tag
  BodyLeaf leaf -> showsPrec prec leaf
  where
    paren innerPrec = showParen (prec > innerPrec)

showsPrecBodyExpr :: (Show def, Show a) => Int -> BodyExpr def a -> ShowS
showsPrecBodyExpr = showsPrecBody exprHasGetVar

showBodyExpr :: BodyExpr String String -> String
showBodyExpr = flip (showsPrecBodyExpr 0) ""

instance (Show def, Show expr) => Show (Body def expr) where
  showsPrec = showsPrecBody mayDepend
    where
      -- We are polymorphic on any expr, so we cannot tell...
      mayDepend _ _ = True

instance (Show def, Show a) => Show (Expression def a) where
  showsPrec prec (Expression body payload) =
    showsPrecBodyExpr bodyPrec body .
    showString showPayload
    where
      (bodyPrec, showPayload) =
        case show payload of
        "" -> (prec, "")
        "()" -> (prec, "")
        str -> (11, "{" ++ str ++ "}")
