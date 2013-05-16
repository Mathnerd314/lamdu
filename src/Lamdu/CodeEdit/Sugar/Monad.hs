{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell #-}
module Lamdu.CodeEdit.Sugar.Monad
  ( Context(..)
  , scMDefI, scInferState, scMContextHash, scHoleInferState
  , scCodeAnchors, scSpecialFunctions, scMReinferRoot
  , mkContext
  , SugarM(..), run, runPure
  , readContext, liftTransaction
  , codeAnchor
  , getP
  ) where

import Control.Applicative (Applicative, (<$>))
import Control.Lens ((^.))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.MonadA (MonadA)
import Data.Store.IRef (Tag)
import Data.Typeable (Typeable)
import Lamdu.CodeEdit.Sugar.Infer (InferLoadedResult, ilrInferContext, ilrContext, ilrBaseInferContext)
import Lamdu.CodeEdit.Sugar.Types -- see export list
import Lamdu.Data.Expression.IRef (DefI)
import qualified Control.Lens.TH as LensTH
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Cache as Cache
import qualified Data.Store.Transaction as Transaction
import qualified Lamdu.Data.Anchors as Anchors
import qualified Lamdu.Data.Expression.Infer as Infer

data Context m = Context
  { _scMDefI :: Maybe (DefI (Tag m))
  , _scInferState :: Infer.Context (DefI (Tag m))
  , _scMContextHash :: Maybe Cache.KeyBS -- Nothing if converting pure expression
  , _scHoleInferState :: Infer.Context (DefI (Tag m))
  , _scCodeAnchors :: Anchors.CodeProps m
  , _scSpecialFunctions :: Anchors.SpecialFunctions (Tag m)
  , _scMReinferRoot :: Maybe (String -> CT m Bool)
  }
LensTH.makeLenses ''Context

newtype SugarM m a = SugarM (ReaderT (Context m) (T m) a)
  deriving (Functor, Applicative, Monad)

mkContext ::
  MonadA m => Typeable (m ()) =>
  Anchors.CodeProps m ->
  Maybe (DefI (Tag m)) ->
  Maybe (String -> CT m Bool) ->
  InferLoadedResult m ->
  T m (Context m)
mkContext cp mDefI mReinferRoot iResult = do
  specialFunctions <- Transaction.getP $ Anchors.specialFunctions cp
  return Context
    { _scMDefI = mDefI
    , _scInferState = iResult ^. ilrInferContext
    , _scMContextHash = Just . Cache.bsOfKey $ iResult ^. ilrContext
    , _scHoleInferState = iResult ^. ilrBaseInferContext
    , _scCodeAnchors = cp
    , _scSpecialFunctions = specialFunctions
    , _scMReinferRoot = mReinferRoot
    }

run :: MonadA m => Context m -> SugarM m a -> T m a
run ctx (SugarM action) = runReaderT action ctx

runPure :: MonadA m => Anchors.CodeProps m -> SugarM m a -> T m a
runPure cp act = do
  specialFunctions <- Transaction.getP $ Anchors.specialFunctions cp
  run Context
    { _scMDefI = Nothing
    , _scInferState = error "pure expression doesnt have infer state"
    , _scHoleInferState = error "pure expression doesnt have hole infer state"
    , _scMContextHash = Nothing
    , _scCodeAnchors = cp
    , _scSpecialFunctions = specialFunctions
    , _scMReinferRoot = Nothing
    } act

readContext :: MonadA m => SugarM m (Context m)
readContext = SugarM Reader.ask

liftTransaction :: MonadA m => T m a -> SugarM m a
liftTransaction = SugarM . lift

codeAnchor :: MonadA m => (Anchors.CodeProps m -> a) -> SugarM m a
codeAnchor f = f . (^. scCodeAnchors) <$> readContext

getP :: MonadA m => Transaction.MkProperty m a -> SugarM m a
getP = liftTransaction . Transaction.getP
