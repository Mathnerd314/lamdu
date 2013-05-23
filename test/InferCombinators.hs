{-# OPTIONS -Wall -Werror #-}
module InferCombinators where

import Control.Arrow ((&&&))
import Control.Lens.Operators
import Control.Monad (void)
import Data.Map ((!))
import Data.Store.Guid (Guid)
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Expression(..), Kind(..))
import Lamdu.Data.Expression.IRef (DefI)
import Utils
import qualified Control.Lens as Lens
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type InferResults t =
  ExprIRef.Expression t
  ( PureExprDefI t
  , PureExprDefI t
  )

getDef :: String -> InferResults t
getDef name =
  leafSimple
  (Expr.GetVariable . Expr.DefinitionRef $ IRef.unsafeFromGuid g)
  (void (definitionTypes ! g))
  where
    g = Guid.fromString name

leafTag :: Guid -> InferResults t
leafTag guid =
  leafSimple (Expr.Tag guid) .
  ExprUtil.pureExpression $ Expr.BodyLeaf Expr.TagType

defParamTags :: String -> [Guid]
defParamTags defName =
  innerMostPi (definitionTypes ! g) ^..
  ExprLens.exprLam . Expr.lambdaParamType .
  ExprLens.exprRecord . Expr.recordFields .
  Lens.traversed . Lens._1 . ExprLens.exprTag
  where
    g = Guid.fromString defName

innerMostPi :: Expression def a -> Expression def a
innerMostPi =
  last . pis
  where
    pis expr =
      case expr ^? ExprLens.exprLam of
      Just lam
        | lam ^. Expr.lambdaKind == Type ->
          expr : pis (lam ^. Expr.lambdaResult)
      _ -> []

getParam :: String -> PureExprDefI t -> InferResults t
getParam name = leafSimple gv
  where
    gv = Expr.GetVariable . Expr.ParameterRef $ Guid.fromString name

inferResults :: ExprIRef.Expression t (Infer.Inferred (DefI t)) -> InferResults t
inferResults = fmap (void . Infer.iValue &&& void . Infer.iType)

leafSimple :: Expr.Leaf (DefI t) -> PureExprDefI t -> InferResults t
leafSimple l =
  leaf l . ExprUtil.pureExpression $ Expr.BodyLeaf l

leaf ::
  Expr.Leaf (DefI t) ->
  PureExprDefI t -> PureExprDefI t ->
  InferResults t
leaf l val typ = iexpr val typ $ Expr.BodyLeaf l

iexpr ::
  PureExprDefI t ->
  PureExprDefI t ->
  Expr.Body (DefI t) (InferResults t) -> InferResults t
iexpr iVal iType body =
  Expr.Expression body (iVal, iType)

five :: PureExprDefI t
five = ExprLens.pureExpr . ExprLens.bodyLiteralInteger # 5

inferredHole :: PureExprDefI t -> InferResults t
inferredHole = leafSimple Expr.Hole

inferredSetType :: InferResults t
inferredSetType = leafSimple Expr.Set setType

bodyToPureExpr :: Expr.Body def (Expression def a) -> PureExpr def
bodyToPureExpr exprBody = ExprLens.pureExpr # fmap void exprBody

makeApply :: [InferResults t] -> InferResults t
makeApply = foldl1 step
  where
    step func@(Expr.Expression _ (funcVal, funcType)) nextArg =
      iexpr applyVal applyType apply
      where
        apply = ExprUtil.makeApply func nextArg
        handleLam e k seeHole seeOther =
          case e ^. Expr.eBody of
          Expr.BodyLeaf Expr.Hole -> seeHole
          Expr.BodyLam (Expr.Lambda k1 paramGuid _ result)
            | k == k1 -> ExprUtil.subst paramGuid (void nextArg) result
          _ -> seeOther
        applyVal = handleLam funcVal Val pureHole $ bodyToPureExpr apply
        applyType = handleLam funcType Type piErr piErr
        piErr = error "Apply of non-Pi type!"

record :: Kind -> [(InferResults t, InferResults t)] -> InferResults t
record k fields =
  iexpr (bodyToPureExpr (recBody k)) typ $ recBody k
  where
    typ = case k of
      Val -> bodyToPureExpr $ recBody Type
      Type -> setType
    recBody k1 = ExprLens.bodyKindedRecordFields k1 # fields

inferredVal :: InferResults t -> InferResults t
inferredVal expr =
  iexpr val typ $ ExprLens.bodyHole # ()
  where
    (val, typ) = expr ^. Expr.ePayload
