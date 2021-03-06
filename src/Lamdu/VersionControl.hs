module Lamdu.VersionControl (makeActions, runAction, runEvent) where

import Control.Lens.Operators
import Control.Monad (unless)
import Data.List (elemIndex)
import Data.List.Utils (removeAt)
import Data.Maybe (fromMaybe)
import Data.Store.IRef (Tag)
import Data.Store.Rev.Branch (Branch)
import Data.Store.Rev.View (View)
import Data.Store.Transaction (Transaction, setP, getP, modP)
import Lamdu.Anchors (DbM)
import Lamdu.VersionControl.Actions (Actions(Actions))
import qualified Data.Store.Rev.Branch as Branch
import qualified Data.Store.Rev.Version as Version
import qualified Data.Store.Rev.View as View
import qualified Data.Store.Transaction as Transaction
import qualified Graphics.UI.Bottle.Widget as Widget
import qualified Lamdu.Anchors as Anchors
import qualified Lamdu.VersionControl.Actions as Actions

-- TODO: Use the monad newtypes:
type TDB = Transaction DbM
type TV = Transaction Anchors.ViewM

revProp :: (Anchors.RevisionProps -> a) -> a
revProp x = x Anchors.revisionProps

codeProp :: (Anchors.CodeProps -> a) -> a
codeProp x = x Anchors.codeProps

setCurrentBranch :: View (Tag DbM) -> Branch (Tag DbM) -> TDB ()
setCurrentBranch view branch = do
  setP (revProp Anchors.currentBranch) branch
  View.setBranch view branch

deleteBranch :: View (Tag DbM) -> [Branch (Tag DbM)] -> Branch (Tag DbM) -> TDB (Branch (Tag DbM))
deleteBranch view branches branch = do
  setP (revProp Anchors.branches) newBranches
  setCurrentBranch view newBranch
  return newBranch
  where
    newBranch = newBranches !! min (length newBranches - 1) index
    index =
      fromMaybe (error "Invalid current branch!") $
      elemIndex branch branches
    newBranches = removeAt index branches

makeBranch :: View (Tag DbM) -> TDB (Branch (Tag DbM))
makeBranch view = do
  newBranch <- Branch.new =<< View.curVersion view
  modP (revProp Anchors.branches) (++ [newBranch])
  setCurrentBranch view newBranch
  return newBranch

runAction :: TV a -> TDB a
runAction action = do
  view <- getP $ revProp Anchors.view
  Anchors.runViewTransaction view action

runEvent :: Widget.Id -> TV Widget.EventResult -> TDB Widget.EventResult
runEvent preCursor eventHandler = do
  (eventResult, isEmpty) <- runAction $ do
    eventResult <- eventHandler
    isEmpty <- Transaction.isEmpty
    unless isEmpty $ do
      setP (codeProp Anchors.preCursor) preCursor
      setP (codeProp Anchors.postCursor) .
        fromMaybe preCursor $ eventResult ^. Widget.eCursor
    return (eventResult, isEmpty)
  unless isEmpty $ setP (revProp Anchors.redos) []
  return eventResult

makeActions :: Transaction DbM (Actions (Tag DbM) (Transaction DbM))
makeActions = do
  view <- getP $ revProp Anchors.view
  branches <- getP $ revProp Anchors.branches
  currentBranch <- getP $ revProp Anchors.currentBranch
  curVersion <- View.curVersion view
  curVersionData <- Version.versionData curVersion
  allRedos <- getP $ revProp Anchors.redos
  let
    toDb = Anchors.runViewTransaction view
    undo parentVersion = do
      preCursor <- toDb . getP $ codeProp Anchors.preCursor
      View.move view parentVersion
      modP (revProp Anchors.redos) (curVersion :)
      return preCursor
    mkRedo [] = Nothing
    mkRedo (redo : redos) = Just $ do
      setP (revProp Anchors.redos) redos
      View.move view redo
      toDb . getP $ codeProp Anchors.postCursor
  return Actions
    { Actions.branches = branches
    , Actions.currentBranch = currentBranch
    , Actions.setCurrentBranch = setCurrentBranch view
    , Actions.deleteBranch = deleteBranch view branches
    , Actions.makeBranch = makeBranch view
    , Actions.mUndo = fmap undo $ Version.parent curVersionData
    , Actions.mRedo = mkRedo allRedos
    }
