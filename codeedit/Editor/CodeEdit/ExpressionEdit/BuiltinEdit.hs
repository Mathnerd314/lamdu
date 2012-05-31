module Editor.CodeEdit.ExpressionEdit.BuiltinEdit(make) where

import Data.List.Split (splitOn)
import Data.Store.Property (Property(..))
import Editor.Anchors (ViewTag)
import Editor.CTransaction (TWidget, assignCursor)
import Editor.MonadF (MonadF)
import qualified Data.List as List
import qualified Editor.BottleWidgets as BWidgets
import qualified Editor.CodeEdit.Sugar as Sugar
import qualified Editor.Config as Config
import qualified Editor.Data as Data
import qualified Editor.WidgetIds as WidgetIds
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Graphics.UI.Bottle.Widgets.FocusDelegator as FocusDelegator

make
  :: MonadF m
  => Sugar.Builtin m
  -> Widget.Id
  -> TWidget ViewTag m
make (Sugar.Builtin (Data.FFIName modulePath name) setFFIName) myId =
  assignCursor myId (WidgetIds.builtinFFIName myId) $ do
    moduleName <- makeNamePartEditor Config.foreignModuleColor modulePathStr modulePathSetter WidgetIds.builtinFFIPath
    varName <- makeNamePartEditor Config.foreignVarColor name nameSetter WidgetIds.builtinFFIName
    dot <- BWidgets.makeLabel "." $ Widget.toAnimId myId
    return $ BWidgets.hbox [moduleName, dot, varName]
  where
    makeNamePartEditor color namePartStr mSetter makeWidgetId =
      BWidgets.setTextColor color .
      BWidgets.wrapDelegated FocusDelegator.NotDelegating
      (maybe
       (BWidgets.makeTextView namePartStr . Widget.toAnimId)
       (BWidgets.makeWordEdit . Property (return namePartStr)) mSetter) $
      makeWidgetId myId
    maybeSetter f = fmap f setFFIName
    modulePathStr = List.intercalate "." modulePath
    modulePathSetter = maybeSetter $ \ffiNameSetter ->
      ffiNameSetter . (`Data.FFIName` name) . splitOn "."
    nameSetter = maybeSetter $ \ffiNameSetter -> ffiNameSetter . Data.FFIName modulePath