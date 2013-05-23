{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Lamdu.Data.Expression.IRef
  ( Expression, ExpressionM
  , ExpressionI(..), ExpressionIM
  , ExpressionBody
  , ExpressionProperty, epGuid
  , Lambda, Apply
  , newExprBody, readExprBody, writeExprBody, exprGuid
  , newLambda, newPi
  , newExpression, writeExpression, readExpression
  , writeExpressionWithStoredSubexpressions
  , DefI, DefinitionI
  , variableRefGuid
  ) where

import Control.Applicative ((<$>), pure)
import Control.Lens ((^.))
import Control.MonadA (MonadA)
import Data.Binary (Binary(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (IRef, Tag)
import Data.Store.Property (Property)
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Lamdu.Data.Definition (Definition)
import qualified Control.Lens as Lens
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type Expression t = Expr.Expression (DefI t)
type ExpressionM m = Expr.Expression (DefI (Tag m))

type T = Transaction

type DefinitionI t = Definition (ExpressionI t)
type DefI t = IRef t (DefinitionI t)

newtype ExpressionI t = ExpressionI {
  unExpression :: IRef t (Expr.Body (DefI t) (ExpressionI t))
  } deriving (Eq, Ord, Show, Typeable, Binary)

type ExpressionIM m = ExpressionI (Tag m)

type ExpressionProperty m = Property (T m) (ExpressionIM m)
type ExpressionBody t = Expr.Body (DefI t) (ExpressionI t)
type Lambda t = Expr.Lambda (ExpressionI t)
type Apply t = Expr.Apply (ExpressionI t)

epGuid :: ExpressionProperty m -> Guid
epGuid = IRef.guid . unExpression . Property.value

exprGuid :: ExpressionI t -> Guid
exprGuid = IRef.guid . unExpression

newExprBody :: MonadA m => ExpressionBody (Tag m) -> T m (ExpressionIM m)
newExprBody = fmap ExpressionI . Transaction.newIRef

newLambdaCons ::
  MonadA m =>
  (Guid -> expr -> expr -> ExpressionBody (Tag m)) ->
  expr -> expr -> T m (Guid, ExpressionIM m)
newLambdaCons cons paramType result = do
  key <- Transaction.newKey
  expr <- newExprBody $ cons key paramType result
  return (key, expr)

newPi :: MonadA m => ExpressionIM m -> ExpressionIM m -> T m (Guid, ExpressionIM m)
newPi = newLambdaCons ExprUtil.makePi

newLambda :: MonadA m => ExpressionIM m -> ExpressionIM m -> T m (Guid, ExpressionIM m)
newLambda = newLambdaCons ExprUtil.makeLambda

readExprBody :: MonadA m => ExpressionIM m -> T m (ExpressionBody (Tag m))
readExprBody = Transaction.readIRef . unExpression

writeExprBody :: MonadA m => ExpressionIM m -> ExpressionBody (Tag m) -> T m ()
writeExprBody = Transaction.writeIRef . unExpression

newExpression :: MonadA m => ExpressionM m () -> T m (ExpressionIM m)
newExpression =
  fmap (fst . Lens.view Expr.ePayload) .
  newExpressionFromH . ((,) Nothing <$>)

-- Returns expression with new Guids
writeExpression ::
  MonadA m => ExpressionIM m -> ExpressionM m a ->
  T m (ExpressionM m (ExpressionIM m, a))
writeExpression iref =
  writeExpressionWithStoredSubexpressions iref .
  fmap ((,) Nothing)

writeExpressionWithStoredSubexpressions ::
  MonadA m =>
  ExpressionIM m ->
  ExpressionM m (Maybe (ExpressionIM m), a) ->
  T m (ExpressionM m (ExpressionIM m, a))
writeExpressionWithStoredSubexpressions iref expr = do
  exprBodyP <- expressionBodyFrom expr
  writeExprBody iref $ fmap (fst . Lens.view Expr.ePayload) exprBodyP
  return $ Expr.Expression exprBodyP
    (iref, expr ^. Expr.ePayload . Lens._2)

readExpression ::
  MonadA m => ExpressionIM m -> T m (ExpressionM m (ExpressionIM m))
readExpression exprI =
  fmap (`Expr.Expression` exprI) .
  traverse readExpression =<< readExprBody exprI

expressionBodyFrom ::
  MonadA m =>
  ExpressionM m (Maybe (ExpressionIM m), a) ->
  T m (Expr.BodyExpr (DefI (Tag m)) (ExpressionIM m, a))
expressionBodyFrom = traverse newExpressionFromH . Lens.view Expr.eBody

newExpressionFromH ::
  MonadA m =>
  ExpressionM m (Maybe (ExpressionIM m), a) ->
  T m (ExpressionM m (ExpressionIM m, a))
newExpressionFromH expr =
  case Lens.sequenceOf (Lens.traverse . Lens._1) expr of
  Just result -> pure result
  Nothing -> f <$> Transaction.newIRefWithGuid (const mkPair)
  where
    mkPair = do
      body <- expressionBodyFrom expr
      pure (Lens.view (Expr.ePayload . Lens._1) <$> body, body)
    f (exprI, body) =
      Expr.Expression body
      ( ExpressionI exprI
      , expr ^. Expr.ePayload . Lens._2
      )

variableRefGuid :: Expr.VariableRef (DefI t) -> Guid
variableRefGuid (Expr.ParameterRef i) = i
variableRefGuid (Expr.DefinitionRef i) = IRef.guid i
