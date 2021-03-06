{-# LANGUAGE TemplateHaskell, DeriveFunctor, ConstraintKinds #-}
module Lamdu.CodeEdit.Sugar.Infer
  ( Payload(..), plGuid, plInferred, plStored, plData
  , ExpressionSetter
  , NoInferred(..), InferredWC
  , NoStored(..), Stored
  , PayloadMM, ExprMM

  , isPolymorphicFunc
  , inferAddImplicits
  , InferredWithImplicits(..)
  , iwiSuccess, iwiBaseInferContextKey, iwiInferContext
  , iwiExpr, iwiBaseExpr, iwiBaseInferContext

  , mkExprPure

  -- TODO: These don't belong here:
  -- Type-check an expression into an ordinary Inferred Expression,
  -- short-circuit on error:
  , load, inferMaybe, inferMaybe_
  , memoLoadInfer

  , exprInferred
  , exprStored
  , exprGuid
  , exprStoredGuid

  , plIRef
  , exprIRef
  , replaceWith
  ) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Lens (Lens')
import Control.Lens.Operators
import Control.Monad (void, (<=<))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT(..), evalStateT)
import Control.Monad.Trans.State.Utils (toStateT)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Maybe (isJust)
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Data.Typeable (Typeable, Typeable1)
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Conflicts (InferredWithConflicts(..), inferWithConflicts)
import System.Random (RandomGen)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.State as State
import qualified Data.Cache as Cache
import qualified Data.Store.Property as Property
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.IRef as ExprIRef
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Infer.ImplicitVariables as ImplicitVariables
import qualified Lamdu.Data.Expression.Infer.Structure as Structure
import qualified Lamdu.Data.Expression.Load as Load
import qualified Lamdu.Data.Expression.Utils as ExprUtil

type T = Transaction
type CT m = StateT Cache (T m)

data NoInferred = NoInferred
type InferredWC t = InferredWithConflicts (DefI t)

data NoStored = NoStored
type Stored m = ExprIRef.ExpressionProperty m

type ExpressionSetter def = Expr.Expression def () -> Expr.Expression def ()

data Payload inferred stored a
  = Payload
    { _plGuid :: Guid
    , _plInferred :: inferred
    , _plStored :: stored
    , _plData :: a
    }
Lens.makeLenses ''Payload

type PayloadMM m a =
  Payload (Maybe (InferredWC (Tag m))) (Maybe (Stored m)) a
type ExprMM m a = ExprIRef.ExpressionM m (PayloadMM m a)

-- Not inferred, not stored
mkExprPure ::
  RandomGen g => g -> ExprIRef.ExpressionM m a -> ExprMM m a
mkExprPure g =
  ExprUtil.randomizeExpr g . fmap f
  where
    f a guid = Payload guid Nothing Nothing a

-- {{{{{{{{{{{{{{{{{
-- TODO: These don't belong here
loader :: MonadA m => Infer.Loader (DefI (Tag m)) (T m)
loader =
  Infer.Loader
  (fmap void . ExprIRef.readExpression . (^. Definition.defType) <=<
   Transaction.readIRef)

load ::
  MonadA m => Maybe (DefI (Tag m)) ->
  ExprIRef.ExpressionM m a ->
  T m (Infer.Loaded (DefI (Tag m)) a)
load = Infer.load loader

-- TODO: All uses of inferMaybe wrap it with memoization, so put
-- memoization here...
inferMaybe ::
  MonadA m =>
  Infer.Loaded (DefI (Tag m)) a ->
  Infer.Context (DefI (Tag m)) ->
  Infer.InferNode (DefI (Tag m)) ->
  Maybe
  ( ExprIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)), a)
  , Infer.Context (DefI (Tag m))
  )
inferMaybe loaded inferContext inferPoint =
  (`runStateT` inferContext) $
  Infer.inferLoaded (Infer.InferActions (const Nothing))
  loaded inferPoint

pureMemoBy ::
  (Cache.Key k, Binary v, MonadA m) =>
  k -> v -> StateT Cache m v
pureMemoBy k val = Cache.memoS (const (return val)) k

-- memoLoadInfer does an infer "load" of the given expression (which
-- loads dependent expression types), and then memoizes the inference
-- result of that in a given context. It uses the "loaded" expression
-- (expr+dependent def types) and a given "inferStateKey" as the
-- memoization key. This is done because using the "inferState"
-- directly as a key could potentially be huge and
-- wasteful. Therefore, the caller is in charge of giving us a unique
-- identifier for the inferState that is preferably small.
memoLoadInfer ::
  (MonadA m, Typeable1 m, Cache.Key a) => Maybe (DefI (Tag m)) ->
  ExprIRef.ExpressionM m a -> Cache.KeyBS ->
  ( Infer.Context (DefI (Tag m))
  , Infer.InferNode (DefI (Tag m))
  ) ->
  CT m
  ( Maybe
    ( ExprIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)), a)
    , Infer.Context (DefI (Tag m))
    )
  )
memoLoadInfer mDefI expr inferStateKey (inferState, point) = do
  loaded <- lift $ load mDefI expr
  pureMemoBy (loaded, inferStateKey, point) $
    inferMaybe loaded inferState point

inferMaybe_ ::
  MonadA m =>
  Infer.Loaded (DefI (Tag m)) a ->
  Infer.Context (DefI (Tag m)) ->
  Infer.InferNode (DefI (Tag m)) ->
  Maybe
  ( ExprIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)))
  , Infer.Context (DefI (Tag m))
  )
inferMaybe_ loaded inferContext inferPoint =
  (Lens._Just . Lens._1 . Lens.traversed %~ fst) $ inferMaybe loaded inferContext inferPoint

inferWithVariables ::
  (RandomGen g, MonadA m) => g ->
  Infer.Loaded (DefI (Tag m)) a -> Infer.Context (DefI (Tag m)) -> Infer.InferNode (DefI (Tag m)) ->
  T m
  ( ( Infer.Context (DefI (Tag m))
    , ExprIRef.ExpressionM m (InferredWithConflicts (DefI (Tag m)), a)
    )
  , Maybe
    ( Infer.Context (DefI (Tag m))
    , ExprIRef.ExpressionM m (InferredWithConflicts (DefI (Tag m)), ImplicitVariables.Payload a)
    )
  )
inferWithVariables gen loaded baseInferContext node =
  (`evalStateT` baseInferContext) $ do
    (success, expr) <- toStateT $ inferWithConflicts loaded node
    intermediateContext <- State.get
    mWithVariables <- if success
      then do
        wvExpr <-
          Structure.add loader =<<
          ImplicitVariables.add gen loader
          ((iwcInferred . fst &&& id) <$> expr)
        wvContext <- State.get
        return $ Just (wvContext, asIWC <$> wvExpr)
      else
        return Nothing
    return
      ( (intermediateContext, expr)
      , mWithVariables
      )
  where
    asIWC (newInferred, ImplicitVariables.Stored (oldIWC, a)) =
      ( oldIWC { iwcInferred = newInferred }
      , ImplicitVariables.Stored a
      )
    asIWC (newInferred, ImplicitVariables.AutoGen guid) =
      ( InferredWithConflicts newInferred [] []
      , ImplicitVariables.AutoGen guid
      )

data InferredWithImplicits m a = InferredWithImplicits
  { _iwiSuccess :: Bool
  , _iwiInferContext :: Infer.Context (DefI (Tag m))
  , _iwiExpr :: ExprIRef.ExpressionM m (Payload (InferredWC (Tag m)) (Maybe (Stored m)) a)
  -- Prior to adding variables
  , _iwiBaseInferContext :: Infer.Context (DefI (Tag m))
  , _iwiBaseInferContextKey :: Cache.KeyBS
  , _iwiBaseExpr :: ExprIRef.ExpressionM m (Payload (InferredWC (Tag m)) (Stored m) a)
  }
Lens.makeLenses ''InferredWithImplicits

inferAddImplicits ::
  (RandomGen g, MonadA m, Typeable (m ())) => g ->
  Maybe (DefI (Tag m)) -> Load.LoadedClosure (Tag m) ->
  Cache.KeyBS ->
  ( Infer.Context (DefI (Tag m))
  , Infer.InferNode (DefI (Tag m))
  ) -> CT m (InferredWithImplicits m ())
inferAddImplicits gen mDefI lExpr inferContextKey inferState = do
  loaded <- lift $ load mDefI lExpr
  ((baseContext, expr), mWithVariables) <-
    Cache.memoS uncurriedInfer (loaded, inferState)
  let baseExpr = mkStoredPayload <$> expr
  return InferredWithImplicits
    { _iwiSuccess = isJust mWithVariables
    , _iwiBaseInferContextKey = Cache.bsOfKey (loaded, inferContextKey)

    , _iwiBaseInferContext = baseContext
    , _iwiBaseExpr = baseExpr

    , _iwiInferContext = maybe baseContext fst mWithVariables
    , _iwiExpr =
      maybe (baseExpr & Lens.mapped . plStored %~ Just)
      (fmap mkWVPayload . snd) mWithVariables
    }
  where
    uncurriedInfer (loaded, (inferContext, inferNode)) =
      inferWithVariables gen loaded inferContext inferNode

    mkStoredPayload (iwc, propClosure) =
      Payload (ExprIRef.epGuid prop) iwc prop ()
      where
        prop = Load.propertyOfClosure propClosure
    mkWVPayload (iwc, ImplicitVariables.AutoGen guid) =
      Payload guid iwc Nothing ()
    mkWVPayload (iwc, ImplicitVariables.Stored propClosure) =
      mkStoredPayload (iwc, propClosure)
      & plStored %~ Just

isPolymorphicFunc :: PayloadMM m a -> Bool
isPolymorphicFunc funcPl =
  maybe False
  (ExprUtil.isDependentPi . Infer.iType . iwcInferred) $
  funcPl ^. plInferred

exprGuid ::
  Lens' (Expr.Expression def (Payload inferred stored a)) Guid
exprGuid = Expr.ePayload . plGuid

exprStored ::
  Lens' (Expr.Expression def (Payload inferred stored a)) stored
exprStored = Expr.ePayload . plStored

exprInferred ::
  Lens' (Expr.Expression def (Payload inferred stored a)) inferred
exprInferred = Expr.ePayload . plInferred

plIRef ::
  Lens.Traversal' (Payload i (Maybe (Stored m)) a) (ExprIRef.ExpressionIM m)
plIRef = plStored . Lens._Just . Property.pVal

exprStoredGuid ::
  Lens.Fold
  (Expr.Expression def (Payload i (Maybe (Stored m)) a)) Guid
exprStoredGuid = exprIRef . Lens.to ExprIRef.exprGuid

replaceWith :: MonadA m => Stored m -> Stored m -> T m Guid
replaceWith parentP replacerP = do
  Property.set parentP replacerI
  return $ ExprIRef.exprGuid replacerI
  where
    replacerI = Property.value replacerP

exprIRef ::
  Lens.Traversal'
  (Expr.Expression def (Payload i (Maybe (Stored m)) a))
  (ExprIRef.ExpressionIM m)
exprIRef = exprStored . Lens._Just . Property.pVal
