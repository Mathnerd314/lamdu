{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, ConstraintKinds, TypeFamilies #-}
module Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad
  ( ExprGuiM, WidgetT, run
  , widgetEnv

  , transaction, atEnv, withFgColor
  , getP, assignCursor, assignCursorPrefix
  , wrapDelegated
  --
  , makeSubexpresion
  --
  , readSettings, readCodeAnchors
  , getCodeAnchor, mkPrejumpPosSaver
  --
  , addResultPicker, listenResultPickers

  , AccessedVars, markVariablesAsUsed, listenUsedVariables
  , IsDependent(..)
  , withParamName, NameSource(..)
  , withNameFromGetVar, withNameFromVarRef
  , getDefName, getGuidName
  , memo, memoT
  , liftMemo, liftMemoT
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Lens ((%~), (&))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS (RWST, runRWST)
import Control.Monad.Trans.State (StateT(..), mapStateT, runState)
import Control.MonadA (MonadA)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Derive.Monoid (makeMonoid)
import Data.DeriveTH (derive)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Graphics.UI.Bottle.Widget (Widget)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.NameGen (NameGen, IsDependent(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Types (ExpressionGui, WidgetT)
import Lamdu.CodeEdit.Settings (Settings)
import Lamdu.WidgetEnvT (WidgetEnvT)
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.RWS as RWS
import qualified Data.Cache as Cache
import qualified Data.Store.Guid as Guid
import qualified Data.Store.IRef as IRef
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.NameGen as NameGen
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Ops as DataOps
import qualified Lamdu.WidgetEnvT as WE

type T = Transaction
type AccessedVars = [Guid]

data Output m = Output
  { oAccessedVars :: AccessedVars
  , oHolePickers :: [Sugar.PrefixAction m]
  }
derive makeMonoid ''Output

data Askable m = Askable
  { _aNameGen :: NameGen Guid
  , _aSettings :: Settings
  , _aMakeSubexpression :: Sugar.Expression m -> ExprGuiM m (ExpressionGui m)
  , _aCodeAnchors :: Anchors.CodeProps m
  }

newtype ExprGuiM m a = ExprGuiM
  { _exprGuiM :: RWST (Askable m) (Output m) Cache (WidgetEnvT (T m)) a
  }
  deriving (Functor, Applicative, Monad)

LensTH.makeLenses ''Askable
LensTH.makeLenses ''ExprGuiM

atEnv :: MonadA m => (WE.Env -> WE.Env) -> ExprGuiM m a -> ExprGuiM m a
atEnv = Lens.over exprGuiM . RWS.mapRWST . WE.atEnv

withFgColor :: MonadA m => Draw.Color -> ExprGuiM m a -> ExprGuiM m a
withFgColor = atEnv . WE.setTextColor

readSettings :: MonadA m => ExprGuiM m Settings
readSettings = ExprGuiM $ Lens.view aSettings

readCodeAnchors :: MonadA m => ExprGuiM m (Anchors.CodeProps m)
readCodeAnchors = ExprGuiM $ Lens.view aCodeAnchors

mkPrejumpPosSaver :: MonadA m => ExprGuiM m (T m ())
mkPrejumpPosSaver =
  DataOps.savePreJumpPosition <$> readCodeAnchors <*> widgetEnv WE.readCursor

makeSubexpresion :: MonadA m => Sugar.Expression m -> ExprGuiM m (ExpressionGui m)
makeSubexpresion expr = do
  maker <- ExprGuiM $ Lens.view aMakeSubexpression
  maker expr

liftMemo :: MonadA m => StateT Cache (WidgetEnvT (T m)) a -> ExprGuiM m a
liftMemo act = ExprGuiM $ do
  cache <- RWS.get
  (val, newCache) <- lift $ runStateT act cache
  RWS.put newCache
  return val

liftMemoT :: MonadA m => StateT Cache (T m) a -> ExprGuiM m a
liftMemoT = liftMemo . mapStateT lift

memo ::
  (Cache.Key k, Binary v, MonadA m) =>
  (k -> WidgetEnvT (T m) v) -> k -> ExprGuiM m v
memo f key = liftMemo $ Cache.memoS f key

memoT ::
  (Cache.Key k, Binary v, MonadA m) =>
  (k -> T m v) -> k -> ExprGuiM m v
memoT f = memo (lift . f)

run ::
  MonadA m =>
  (Sugar.Expression m -> ExprGuiM m (ExpressionGui m)) ->
  Anchors.CodeProps m -> Settings -> ExprGuiM m a ->
  StateT Cache (WidgetEnvT (T m)) a
run makeSubexpression codeAnchors settings (ExprGuiM action) = StateT $ \cache ->
  fmap f $ runRWST action
  Askable
  { _aNameGen = NameGen.initial
  , _aSettings = settings
  , _aMakeSubexpression = makeSubexpression
  , _aCodeAnchors = codeAnchors
  }
  cache
  where
    f (x, newCache, _) = (x, newCache)

widgetEnv :: MonadA m => WidgetEnvT (T m) a -> ExprGuiM m a
widgetEnv = ExprGuiM . lift

transaction :: MonadA m => T m a -> ExprGuiM m a
transaction = widgetEnv . lift

getP :: MonadA m => Transaction.MkProperty m a -> ExprGuiM m a
getP = transaction . Transaction.getP

getCodeAnchor ::
  MonadA m => (Anchors.CodeProps m -> Transaction.MkProperty m b) -> ExprGuiM m b
getCodeAnchor anchor = getP . anchor =<< readCodeAnchors

assignCursor :: MonadA m => Widget.Id -> Widget.Id -> ExprGuiM m a -> ExprGuiM m a
assignCursor x y = atEnv $ WE.envAssignCursor x y

assignCursorPrefix :: MonadA m => Widget.Id -> Widget.Id -> ExprGuiM m a -> ExprGuiM m a
assignCursorPrefix x y = atEnv $ WE.envAssignCursorPrefix x y

wrapDelegated ::
  (MonadA f, MonadA m) =>
  FocusDelegator.Config -> FocusDelegator.IsDelegating ->
  ((Widget f -> Widget f) -> a -> b) ->
  (Widget.Id -> ExprGuiM m a) ->
  Widget.Id -> ExprGuiM m b
wrapDelegated =
  BWidgets.wrapDelegatedWith (widgetEnv WE.readCursor)
  (atEnv . Lens.over WE.envCursor)

-- Used vars:

listener :: MonadA m => (Output m -> b) -> ExprGuiM m a -> ExprGuiM m (a, b)
listener f =
  Lens.over exprGuiM RWS.listen
  & Lens.mapped . Lens.mapped . Lens._2 %~ f

listenUsedVariables :: MonadA m => ExprGuiM m a -> ExprGuiM m (a, [Guid])
listenUsedVariables = listener oAccessedVars

listenResultPickers :: MonadA m => ExprGuiM m a -> ExprGuiM m (a, [T m ()])
listenResultPickers = listener oHolePickers

markVariablesAsUsed :: MonadA m => AccessedVars -> ExprGuiM m ()
markVariablesAsUsed vars = ExprGuiM $ RWS.tell mempty { oAccessedVars = vars }

addResultPicker :: MonadA m => T m () -> ExprGuiM m ()
addResultPicker picker = ExprGuiM $ RWS.tell mempty { oHolePickers = [picker] }

-- Auto-generating names

data NameSource = AutoGeneratedName | StoredName

withParamName :: MonadA m => IsDependent -> Guid -> ((NameSource, String) -> ExprGuiM m a) -> ExprGuiM m a
withParamName isDep guid useName = do
  storedName <- transaction . Transaction.getP $ Anchors.assocNameRef guid
  -- TODO: maybe use Maybe?
  if null storedName
    then do
      nameGen <- ExprGuiM $ Lens.view aNameGen
      let
        (name, newNameGen) =
          runState (NameGen.getName isDep guid) nameGen
      (Lens.over exprGuiM . RWS.local) (Lens.set aNameGen newNameGen) $
        useName (AutoGeneratedName, name)
    else useName (StoredName, storedName)

getDefName :: MonadA m => Guid -> ExprGuiM m (NameSource, String)
getDefName = transaction . getGuidName

getGuidName :: MonadA m => Guid -> T m (NameSource, String)
getGuidName guid = do
  storedName <- Transaction.getP $ Anchors.assocNameRef guid
  return $
    if null storedName
    then (AutoGeneratedName, (("anon_"++) . take 6 . Guid.asHex) guid)
    else (StoredName, storedName)

withNameFrom ::
  MonadA m =>
  Sugar.VarType -> Guid -> ((NameSource, String) -> ExprGuiM m a) -> ExprGuiM m a
withNameFrom varType g useName =
  case varType of
  Sugar.GetParameter ->
    withParamName (error "Invalid ParameterRef to undefined name") g useName
  Sugar.GetDefinition -> useName =<< getDefName g

withNameFromGetVar ::
  MonadA m => Sugar.GetVar f ->
  ((NameSource, String) -> ExprGuiM m a) -> ExprGuiM m a
withNameFromGetVar getVar =
  withNameFrom (Sugar.gvVarType getVar) (Sugar.gvIdentifier getVar)

withNameFromVarRef ::
  MonadA m => Expression.VariableRef (DataIRef.DefI (Tag m)) ->
  ((NameSource, String) -> ExprGuiM m a) -> ExprGuiM m a
withNameFromVarRef (Expression.ParameterRef g) = withNameFrom Sugar.GetParameter g
withNameFromVarRef (Expression.DefinitionRef defI) =
  withNameFrom Sugar.GetDefinition $ IRef.guid defI
