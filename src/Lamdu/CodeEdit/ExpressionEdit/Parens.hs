{-# LANGUAGE OverloadedStrings #-}
module Lamdu.CodeEdit.ExpressionEdit.Parens
  ( addTextParens
  , addHighlightedTextParens
  ) where

import Control.Lens.Operators
import Control.MonadA (MonadA)
import Data.Function (on)
import Data.Vector.Vector2 (Vector2(..))
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui (ExpressionGui)
import Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad (ExprGuiM, WidgetT)
import Lamdu.WidgetIds (parensPrefix)
import qualified Control.Lens as Lens
import qualified Graphics.UI.Bottle.Animation as Anim
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.BottleWidgets as BWidgets
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui as ExpressionGui
import qualified Lamdu.CodeEdit.ExpressionEdit.ExpressionGui.Monad as ExprGuiM
import qualified Lamdu.Config as Config
import qualified Lamdu.Layers as Layers
import qualified Lamdu.WidgetEnvT as WE
import qualified Lamdu.WidgetIds as WidgetIds

type WidgetMaker m = ExprGuiM m (WidgetT m)

addTextParensI
  :: MonadA m
  => (WidgetMaker m -> WidgetMaker m)
  -> (WidgetMaker m -> WidgetMaker m)
  -> Anim.AnimId
  -> ExpressionGui m
  -> ExprGuiM m (ExpressionGui m)
addTextParensI onLParen onRParen parenId gui = do
  beforeParen <- onLParen $ label "("
  afterParen <- onRParen $ label ")"
  let
    wHeight = Widget.wSize . Lens._2
    parenHeight = on max (^. wHeight) beforeParen afterParen
    guiHeight = gui ^. ExpressionGui.egWidget . wHeight
    scaleParen
      | guiHeight > parenHeight =
        Widget.scale . Vector2 1 $ 1.2 * guiHeight / parenHeight
      | otherwise = id
    alignment = gui ^. ExpressionGui.egAlignment
  return $
    ExpressionGui.horizontalAddAround
    [ (Vector2 0 alignment, scaleParen beforeParen) ] gui
    [ (Vector2 0 alignment, scaleParen afterParen) ]
  where
    label str =
      ExprGuiM.widgetEnv . BWidgets.makeLabel str $ parensPrefix parenId

highlightExpression :: Widget.Widget f -> Widget.Widget f
highlightExpression =
  Widget.backgroundColor Layers.parensHighlightBG WidgetIds.parenHighlightId Config.parenHighlightColor

addTextParens
  :: MonadA m
  => Anim.AnimId
  -> ExpressionGui m
  -> ExprGuiM m (ExpressionGui m)
addTextParens = addTextParensI id id

addHighlightedTextParens
  :: (MonadA m)
  => Widget.Id
  -> ExpressionGui m
  -> ExprGuiM m (ExpressionGui m)
addHighlightedTextParens myId widget = do
  mInsideParenId <- ExprGuiM.widgetEnv $ WE.subCursor rParenId
  widgetWithParens <- addTextParensI id doHighlight (Widget.toAnimId myId) widget
  return $
    maybe id (const (Lens.over ExpressionGui.egWidget highlightExpression))
    mInsideParenId widgetWithParens
  where
    rParenId = Widget.joinId myId [")"]
    doHighlight = (ExprGuiM.widgetEnv . BWidgets.makeFocusableView rParenId =<<)
