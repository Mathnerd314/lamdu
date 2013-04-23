{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

module Lamdu.Config where

import qualified Lamdu.WidgetIds as WidgetIds
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.UI.Bottle.EventMap as E
import qualified Graphics.UI.Bottle.Widgets.TextEdit as TextEdit
import qualified Graphics.UI.Bottle.Widgets.TextView as TextView

-- TODO: Oops, we don't want to export these:
mk = E.ModKey

noMods = mk E.noMods
ctrl = mk E.ctrl . E.charKey
alt = mk E.alt . E.charKey
shift = mk E.shift . E.charKey
ctrlAlt = mk (E.noMods {E.modCtrl = True, E.modAlt = True}) . E.charKey
-- altShift = mk E.noMods { E.modAlt = True, E.modShift = True } . E.charKey
k = noMods . E.charKey

quitKeys          = [ctrl 'q']
undoKeys          = [ctrl 'z']
redoKeys          = [ctrl 'y']
makeBranchKeys    = [ctrl 's']

jumpToBranchesKeys = [mk E.ctrl E.KeyF10]

overlayDocKeys    = [noMods E.KeyF1, alt 'h']

addNextParamKeys  = [noMods E.KeySpace]

delBranchKeys     = [alt 'o']

closePaneKeys     = [alt 'w']
movePaneDownKeys  = [mk E.alt E.KeyDown]
movePaneUpKeys    = [mk E.alt E.KeyUp]

replaceKeys       = [alt 'r']

pickResultKeys    = [noMods E.KeyEnter]
pickAndMoveToNextHoleKeys = [noMods E.KeySpace]

jumpToDefinitionKeys = [noMods E.KeyEnter]

delForwardKeys      = [noMods E.KeyDel, mk E.alt E.KeyDel]
delBackwordKeys     = [noMods E.KeyBackspace]
giveAsArgumentKeys  = [k ']']
callWithArgumentKeys = [shift '9']
callWithNextArgumentKeys = [shift '0']
debugModeKeys = [ctrlAlt 'd']

newDefinitionKeys = [alt 'n']

definitionColor = Draw.Color 0.8 0.5 1 1
atomColor = definitionColor
parameterColor = Draw.Color 0.2 0.8 0.9 1
paramOriginColor = Draw.Color 1.0 0.8 0.5 1

literalIntColor = Draw.Color 0 1 0 1

previousCursorKeys = [mk E.alt E.KeyLeft]

holeResultCount = 8
holeResultScaleFactor = 0.75
holeSearchTermScaleFactor = 0.6

fieldHoleResultCount = 3

holeBackgroundColor = Draw.Color 0.3 0 0 1
readOnlyHoleBackgroundColor = Draw.Color 0.5 0.25 0.25 1

makeNewFieldSizeFactor = 0.4
fieldScale = 0.4
fieldTint = Draw.Color 1 1 1 0.6

inferredValueScaleFactor = 0.7
inferredValueTint = Draw.Color 1 1 1 0.6

parenHighlightColor = Draw.Color 0.3 0 1 0.25

lambdaWrapKeys = [k '\\']
addWhereItemKeys = [k 'w']

lambdaColor = Draw.Color 1 0.2 0.2 1
lambdaTextSize = 30

rightArrowColor = Draw.Color 1 0.2 0.2 1
rightArrowTextSize = 30

whereColor = Draw.Color 0.8 0.6 0.1 1
whereScaleFactor = 0.85
whereLabelScaleFactor = whereScaleFactor

foldKeys = [k '-']
unfoldKeys = foldKeys

typeScaleFactor = 0.6
squareParensScaleFactor = 0.96

foreignModuleColor = Draw.Color 1 0.3 0.35 1
foreignVarColor = Draw.Color 1 0.65 0.25 1

cutKeys = [ctrl 'x', k 'x']
pasteKeys = [ctrl 'v', k 'v']

inactiveTintColor = Draw.Color 1 1 1 0.8
activeDefBGColor = Draw.Color 0.04 0.04 0.04 1

inferredTypeTint = inferredValueTint
inferredTypeErrorBGColor = Draw.Color 0.5 0.05 0.05 1
inferredTypeBGColor = Draw.Color 0.05 0.15 0.2 1

-- For definitions
polymorphicForegroundColor = Draw.Color 1 0.4 0.3 1
-- For parameters
polymorphicCompactBGColor = Draw.Color 0.1 0.2 0.25 1
polymorphicExpandedBGColor = Draw.Color 0.18 0.14 0.05 1
polymorphicExpandKey = noMods E.KeyEnter
polymorphicCollapseKey = noMods E.KeyEsc

monomorphicDefOriginForegroundColor = paramOriginColor
polymorphicDefOriginForegroundColor = polymorphicForegroundColor

builtinOriginNameColor = monomorphicDefOriginForegroundColor

cursorBGColor = Draw.Color 0 0 1 0.45

helpStyle font = TextView.Style {
  TextView._styleColor = Draw.Color 1 1 1 1,
  TextView._styleFont = font,
  TextView._styleFontSize = 10
  }

baseStyle font = TextEdit.Style
  { TextEdit._sTextViewStyle =
    TextView.Style
      { TextView._styleColor = Draw.Color 1 1 1 1
      , TextView._styleFont = font
      , TextView._styleFontSize = 25
      }
  , TextEdit._sCursorColor = TextEdit.defaultCursorColor
  , TextEdit._sCursorWidth = TextEdit.defaultCursorWidth
  , TextEdit._sTextCursorId = WidgetIds.textCursorId
  , TextEdit._sBackgroundCursorId = WidgetIds.backgroundCursorId
  , TextEdit._sBackgroundColor = cursorBGColor
  , TextEdit._sEmptyUnfocusedString = ""
  , TextEdit._sEmptyFocusedString = ""
  }

listBracketTextSize = 25
listBracketColor = Draw.Color 0.2 0.8 0.8 1
listCommaTextSize = 25
listCommaColor = Draw.Color 0.2 0.8 0.8 1
enumFromToDotSize = 25
enumFromToDotColor = Draw.Color 0.2 0.8 0.8 1

listAddItemKeys = [k ',']

selectedBranchColor = Draw.Color 0 0.5 0 1

jumpLHStoRHSKeys = [k '`']
jumpRHStoLHSKeys = [k '`']

shrinkBaseFontKeys = [ctrl '-']
enlargeBaseFontKeys = [ctrl '=']

enlargeFactor = 1.1
shrinkFactor = 1.1

defTypeLabelTextSize = 16
defTypeLabelColor = Draw.Color 0.6 0.7 1 1

defTypeBoxSizeFactor = 0.6

acceptInferredTypeKeys = [noMods E.KeySpace, noMods E.KeyEnter]

autoGeneratedNameTint = Draw.Color 0.9 0.8 0.7 1

enterSubexpressionKey = mk E.shift E.KeyRight
leaveSubexpressionKey = mk E.shift E.KeyLeft

replaceInferredValueKey = noMods E.KeyEnter
keepInferredValueKey = noMods E.KeyEsc

nextInfoMode = [noMods E.KeyF7]

operatorChars = "+-*/^=><&|%$:."
alphaNumericChars = ['a'..'z'] ++ ['0'..'9']

recordTypeParensColor = rightArrowColor
recordValParensColor = Draw.Color 0.2 1 0.2 1
recordAddFieldKeys = [k 'a', noMods E.KeySpace]
