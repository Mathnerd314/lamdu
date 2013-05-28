{-# LANGUAGE OverloadedStrings, TypeFamilies #-}
module Lamdu.CodeEdit.ExpressionEdit(make) where

import Control.Applicative ((<$>))
import Control.Lens ((^.))
import Control.Monad ((<=<))
import Control.MonadA (MonadA)
import Data.Monoid (Monoid(..))
import Data.Store.Transaction (Transaction)
import Data.Traversable (traverse)
import Graphics.UI.Bottle.Widget (EventHandlers)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM)
import Lamdu.CodeEdit.ExpressionEdit.HoleEdit (HoleState(..))
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator
import qualified Lamdu.CodeEdit.ExpressionEdit.ApplyEdit as ApplyEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.AtomEdit as AtomEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.CollapsedEdit as CollapsedEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.CodeEdit.ExpressionEdit.LambdaEdit as LambdaEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.GetFieldEdit as GetFieldEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.GetParamsEdit as GetParamsEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.GetVarEdit as GetVarEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.HoleEdit as HoleEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.InferredEdit as InferredEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.LabeledApplyEdit as LabeledApplyEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.ListEdit as ListEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.LiteralEdit as LiteralEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.PiEdit as PiEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.RecordEdit as RecordEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.SectionEdit as SectionEdit
import qualified Lamdu.CodeEdit.ExpressionEdit.TagEdit as TagEdit
import qualified Lamdu.CodeEdit.Settings as Settings
import qualified Lamdu.CodeEdit.Sugar as Sugar
import qualified Lamdu.Config as Config
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

data IsHole = NotAHole | IsAHole

pasteEventMap ::
  MonadA m => Sugar.Hole Sugar.Name m -> Widget.EventHandlers (Transaction m)
pasteEventMap =
  maybe mempty
  (Widget.keysEventMapMovesCursor
   Config.pasteKeys (E.Doc ["Edit", "Paste"]) .
   fmap WidgetIds.fromGuid) .
  (Lens.view Sugar.holePaste <=< Lens.view Sugar.holeMActions)

make :: MonadA m => Sugar.ExpressionN m -> ExprGuiM m (ExpressionGui m)
make sExpr = assignCursor $ do
  ((isHole, widget), resultPickers) <-
    ExprGuiM.listenResultPickers $ makeEditor sExpr exprId
  typeEdits <- traverse make $ payload ^. Sugar.plInferredTypes
  let onReadOnly = Widget.doesntTakeFocus
  exprEventMap <- expressionEventMap isHole resultPickers sExpr
  settings <- ExprGuiM.readSettings
  let
    addInferredTypes =
      case Lens.view Settings.sInfoMode settings of
      Settings.InfoNone -> id
      Settings.InfoTypes ->
        ExpressionGui.addType ExpressionGui.Background exprId
        (map
         ( Widget.tint Config.inferredTypeTint
         . Widget.scale Config.typeScaleFactor
         . Lens.view ExpressionGui.egWidget
         ) typeEdits)
      Settings.InfoExamples -> id
  return .
    Lens.over ExpressionGui.egWidget
    ( maybe onReadOnly (const id) (payload ^. Sugar.plActions)
    . Widget.weakerEvents exprEventMap
    ) .
    addInferredTypes $
    widget
  where
    payload = sExpr ^. Sugar.rPayload
    exprId = WidgetIds.fromGuid $ sExpr ^. Sugar.rGuid
    assignCursor f =
      foldr (`ExprGuiM.assignCursor` exprId) f
      (WidgetIds.fromGuid <$> sExpr ^. Sugar.rHiddenGuids)

makeEditor ::
  MonadA m => Sugar.ExpressionN m -> Widget.Id ->
  ExprGuiM m (IsHole, ExpressionGui m)
makeEditor sExpr =
  case sExpr ^. Sugar.rBody of
  Sugar.BodyInferred i ->
    isAHole (i ^. Sugar.iHole) . InferredEdit.make i $ sExpr ^. Sugar.rGuid
  Sugar.BodyHole hole ->
    isAHole hole . HoleEdit.make hole mNextHole $ sExpr ^. Sugar.rGuid
  Sugar.BodyCollapsed poly ->
    notAHole $ CollapsedEdit.make poly
  Sugar.BodyApply hasParens apply ->
    notAHole $ ApplyEdit.make hasParens apply
  Sugar.BodyLam hasParens lam@(Sugar.Lam Sugar.Type _ _ _) ->
    notAHole $ PiEdit.make hasParens lam
  Sugar.BodyLam hasParens lam@(Sugar.Lam Sugar.Val _ _ _) ->
    notAHole $ LambdaEdit.make hasParens lam
  Sugar.BodySection hasParens section ->
    notAHole $ SectionEdit.make hasParens section
  Sugar.BodyLiteralInteger integer ->
    notAHole $ LiteralEdit.makeInt integer
  Sugar.BodyAtom atom ->
    notAHole $ AtomEdit.make atom
  Sugar.BodyList list ->
    notAHole $ ListEdit.make list
  Sugar.BodyRecord record ->
    notAHole $ RecordEdit.make record
  Sugar.BodyGetField getField ->
    notAHole $ GetFieldEdit.make getField
  Sugar.BodyTag tag ->
    notAHole $ TagEdit.make tag
  Sugar.BodyGetVar gv ->
    notAHole $ GetVarEdit.make gv
  Sugar.BodyGetParams gp ->
    notAHole $ GetParamsEdit.make gp
  Sugar.BodyLabeledApply hasParens la ->
    notAHole $ LabeledApplyEdit.make hasParens la
  where
    isAHole hole =
      (fmap . fmap)
      ((,) IsAHole .
       (Lens.over ExpressionGui.egWidget . Widget.weakerEvents) (pasteEventMap hole))
    notAHole = (fmap . fmap) ((,) NotAHole)
    mNextHole = sExpr ^. Sugar.rPayload . Sugar.plNextHole

expressionEventMap ::
  MonadA m =>
  IsHole -> [Sugar.PrefixAction m] ->
  Sugar.ExpressionN m ->
  ExprGuiM m (EventHandlers (Transaction m))
expressionEventMap isHole resultPickers sExpr =
  maybe (return mempty) (actionsEventMap sExpr isHole resultPickers) $
  sExpr ^. Sugar.rPayload . Sugar.plActions

actionsEventMap ::
  MonadA m =>
  Sugar.ExpressionN m -> IsHole -> [Sugar.PrefixAction m] ->
  Sugar.Actions m ->
  ExprGuiM m (EventHandlers (Transaction m))
actionsEventMap sExpr isHole resultPickers actions = do
  isSelected <- ExprGuiM.widgetEnv . WE.isSubCursor $ WidgetIds.fromGuid exprGuid
  callWithArgsEventMap <-
    if isSelected
    then
      mconcat <$> sequence
      [ maybe mempty
        (mkEventMap Config.callWithArgumentKeys (E.Doc ["Edit", docPrefix ++ "Call with argument"])
         FocusDelegator.delegatingId) <$>
        ExprGuiM.liftMemoT ((actions ^. Sugar.callWithArg) prefix)
      , maybe mempty
        (mkEventMap Config.callWithNextArgumentKeys (E.Doc ["Edit", docPrefix ++ "Add argument"])
         FocusDelegator.delegatingId) <$>
        ExprGuiM.liftMemoT ((actions ^. Sugar.callWithNextArg) prefix)
      ]
    else return mempty
  let
    replace
      | isSelected && isHoleBool = mempty
      | isSelected =
        mkEventMap delKeys (E.Doc ["Edit", "Replace expression"])
        FocusDelegator.delegatingId $ actions ^. Sugar.setToHole
      | otherwise =
        mkEventMap delKeys (E.Doc ["Navigation", "Select parent"])
        FocusDelegator.notDelegatingId $ return exprGuid
  return $ mconcat
    [ callWithArgsEventMap
    , giveAsArg
    , addOperator
    , replace
    , cut
    ]
  where
    exprGuid = sExpr ^. Sugar.rGuid
    docPrefix
      | null resultPickers = ""
      | otherwise = "Pick and "
    prefix = sequence_ resultPickers
    delKeys = Config.replaceKeys ++ Config.delKeys
    giveAsArg =
      Widget.keysEventMapMovesCursor
      Config.giveAsArgumentKeys (E.Doc ["Edit", "Give as argument"]) .
      fmap WidgetIds.fromGuid $
      (actions ^. Sugar.giveAsArg) Sugar.emptyPrefixAction
    addOperator =
      (fmap . fmap) Widget.eventResultFromCursor .
      E.charGroup "Operator" (E.Doc ["Edit", "Apply operator"])
      Config.operatorChars $ \c _isShifted -> do
        targetGuid <- actions ^. Sugar.replaceWithNewHole
        HoleEdit.setHoleStateAndJump
          HoleState
          { _hsSearchTerm = [c]
          , _hsArgument = Just $ sExpr ^. Sugar.rPresugaredExpression
          }
          targetGuid
    cut
      | isHoleBool = mempty
      | otherwise =
        mkEventMap Config.cutKeys (E.Doc ["Edit", "Cut"]) id $
        actions ^. Sugar.cut
    mkEventMap keys doc f =
      Widget.keysEventMapMovesCursor keys doc .
      fmap (f . WidgetIds.fromGuid)
    isHoleBool =
      case isHole of
      NotAHole -> False
      IsAHole -> True
