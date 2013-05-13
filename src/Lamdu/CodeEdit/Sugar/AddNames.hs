{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, RankNTypes, DeriveFunctor, TypeFamilies, FlexibleContexts #-}
module Lamdu.CodeEdit.Sugar.AddNames
  ( addToDef
  ) where

import Control.Applicative (Applicative(..), (<$>), (<$))
import Control.Monad.Trans.Reader (Reader, runReader)
import Control.Monad.Trans.State (runState, evalState)
import Control.Monad.Trans.Writer (Writer, runWriter)
import Control.MonadA (MonadA)
import Control.Lens.Operators
import Data.Map (Map)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Traversable (Traversable, traverse)
import Lamdu.CodeEdit.Sugar.NameGen (NameGen)
import Lamdu.CodeEdit.Sugar.Types
import Prelude hiding (pi)
import qualified Control.Lens as Lens
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.Writer as Writer
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Lamdu.CodeEdit.Sugar.NameGen as NameGen

data CPS m a = CPS { runCPS :: forall r. m r -> m (a, r) }
  deriving (Functor)

instance Functor m => Applicative (CPS m) where
  pure x = CPS $ fmap ((,) x)
  CPS cpsf <*> CPS cpsx =
    CPS (fmap foo . cpsf . cpsx)
    where
      foo (f, (x, r)) = (f x, r)


class (MonadA (TransM m), MonadA m) => MonadNaming m where
  type TransM m :: * -> *
  type OldName m
  type NewName m
  opRun :: m (m res -> res)

  opWithParamName :: NameGen.IsDependent -> Guid -> OldName m -> CPS m (NewName m)
  opWithWhereItemName :: Guid -> OldName m -> CPS m (NewName m)
  opGetParamName :: Guid -> OldName m -> m (NewName m)

  opWithDefName :: Guid -> OldName m -> CPS m (NewName m)
  opDefName :: Guid -> OldName m -> m (NewName m)

  opMakeTagName :: Guid -> OldName m -> m (NewName m)

type StoredName = String
newtype NameCount = NameCount (Map StoredName Int)
instance Monoid NameCount where
  mempty = NameCount Map.empty
  NameCount x `mappend` NameCount y = NameCount $ Map.unionWith (+) x y
evalWriter :: Writer w a -> a
evalWriter = fst . runWriter

-- Pass 0:
data StoredNames = StoredNames
  { storedName :: Maybe StoredName
  , storedNamesWithin :: NameCount
  }
newtype Pass0M (m :: * -> *) a = Pass0M (Writer NameCount a)
  deriving (Functor, Applicative, Monad)
fpTellStoredNames :: MonadA m => NameCount -> Pass0M m ()
fpTellStoredNames = Pass0M . Writer.tell
fpListenStoredNames :: MonadA m => Pass0M m a -> Pass0M m (a, NameCount)
fpListenStoredNames (Pass0M act) = Pass0M $ Writer.listen act
runPass0M :: MonadA m => Pass0M m a -> a
runPass0M (Pass0M act) = evalWriter act

instance MonadA m => MonadNaming (Pass0M m) where
  type TransM (Pass0M m) = m
  type OldName (Pass0M m) = MStoredName
  type NewName (Pass0M m) = StoredNames
  opRun = pure runPass0M
  opWithParamName _ = collectName
  opWithWhereItemName = collectName
  opWithDefName = collectName
  opGetParamName = handleStoredName
  opMakeTagName = handleStoredName
  opDefName = handleStoredName

pass0Result :: MonadA m => NameCount -> Maybe StoredName -> Pass0M m StoredNames
pass0Result storedNameCounts mName =
  StoredNames
  { storedName = mName
  , storedNamesWithin =
    storedNameCounts `mappend` myNameCounts
  }
  <$ fpTellStoredNames myNameCounts
  where
    myNameCounts = NameCount $ maybe Map.empty (`Map.singleton` 1) mName

handleStoredName :: MonadA m => Guid -> MStoredName -> Pass0M m StoredNames
handleStoredName _ = pass0Result mempty

collectName :: MonadA m => Guid -> MStoredName -> CPS (Pass0M m) StoredNames
collectName _ mName = CPS $ \k -> do
  (res, storedNameCounts) <- fpListenStoredNames k
  flip (,) res <$> pass0Result storedNameCounts mName

-- Pass 1:
newtype Pass1M (m :: * -> *) a = Pass1M (Reader (NameGen Guid) a)
  deriving (Functor, Applicative, Monad)
runPass1M :: MonadA m => NameGen Guid -> Pass1M m a -> a
runPass1M initial (Pass1M act) = runReader act initial
spGetNameGen :: Pass1M m (NameGen Guid)
spGetNameGen = Pass1M Reader.ask
spWithNameGen :: NameGen Guid -> Pass1M m a -> Pass1M m a
spWithNameGen newNameGen (Pass1M act) =
  Pass1M $ (Reader.local . const) newNameGen act

makeStoredName :: StoredName -> NameGen Guid -> (Name, NameGen Guid)
makeStoredName storedName nameGen =
  ( Name StoredName storedName
  , NameGen.ban (Set.singleton storedName) nameGen
  )

makeName ::
  NameGen.IsDependent -> Guid -> StoredNames ->
  NameGen Guid -> (Name, NameGen Guid)
makeName _ _ (StoredNames (Just storedName) _) nameGen = makeStoredName storedName nameGen
makeName isDep guid (StoredNames Nothing (NameCount nameCounts)) nameGen =
  runState
  (Name AutoGeneratedName <$>
   NameGen.newName (`Map.notMember` nameCounts) isDep guid)
  nameGen

instance MonadA m => MonadNaming (Pass1M m) where
  type TransM (Pass1M m) = m
  type OldName (Pass1M m) = StoredNames
  type NewName (Pass1M m) = Name
  opRun = do
    nameGen <- spGetNameGen
    pure $ runPass1M nameGen
  opWithParamName = newLocalName
  opWithWhereItemName = newLocalName NameGen.Independent
  opGetParamName _ (StoredNames (Just str) _) = pure $ Name StoredName str
  opGetParamName guid (StoredNames _ _) = do
    nameGen <- spGetNameGen
    pure . Name AutoGeneratedName $
      evalState (NameGen.existingName guid) nameGen
  opMakeTagName = nameByGuid "tag_"
  opWithDefName = newLocalNameByGuid "def_"
  opDefName = nameByGuid "def_"

makeNameByGuid :: Show guid => String -> guid -> StoredNames -> NameGen Guid -> (Name, NameGen Guid)
makeNameByGuid prefix guid (StoredNames Nothing _) curNameGen =
  (makeGuidName prefix guid, curNameGen)
makeNameByGuid _ _ (StoredNames (Just storedName) _) curNameGen =
  makeStoredName storedName curNameGen

newLocalNameHelper :: (NameGen Guid -> (a, NameGen Guid)) -> CPS (Pass1M m) a
newLocalNameHelper nameMaker = CPS $ \k -> do
  curNameGen <- spGetNameGen
  let
    (name, newNameGen) = nameMaker curNameGen
  res <- spWithNameGen newNameGen k
  return (name, res)

newLocalNameByGuid :: String -> Guid -> StoredNames -> CPS (Pass1M m) Name
newLocalNameByGuid prefix guid storedNames = newLocalNameHelper $ makeNameByGuid prefix guid storedNames

newLocalName :: NameGen.IsDependent -> Guid -> StoredNames -> CPS (Pass1M m) Name
newLocalName isDep guid storedNames = newLocalNameHelper $ makeName isDep guid storedNames

nameByGuid :: (Show guid, Applicative f) => String -> guid -> StoredNames -> f Name
nameByGuid _ _ (StoredNames (Just str) _) = pure $ Name StoredName str
nameByGuid prefix guid (StoredNames Nothing _) = pure $ makeGuidName prefix guid

makeGuidName :: Show guid => String -> guid -> Name
makeGuidName prefix guid = Name AutoGeneratedName $ prefix ++ show guid

withFuncParam ::
  MonadNaming m =>
  NameGen.IsDependent ->
  FuncParam (OldName m) (TransM m) (Expression (OldName m) (TransM m)) ->
  CPS m (FuncParam (NewName m) (TransM m) (Expression (NewName m) (TransM m)))
withFuncParam isDep fp@FuncParam{..} = CPS $ \k -> do
  mActions <- traverse toFuncParamActions _fpMActions
  typ <- toExpression _fpType
  (name, res) <-
    case _fpVarKind of
    FuncParameter ->
      runCPS (opWithParamName isDep _fpGuid _fpName) k
    FuncFieldParameter ->
      (,) <$> opMakeTagName _fpGuid _fpName <*> k
  pure
    ( fp
      { _fpName = name
      , _fpMActions = mActions
      , _fpType = typ
      }
    , res
    )

toFunc ::
  MonadNaming m =>
  Func (OldName m) (TransM m) (Expression (OldName m) (TransM m)) ->
  m (Func (NewName m) (TransM m) (Expression (NewName m) (TransM m)))
toFunc func@Func {..} = do
  (depParams, (params, body)) <-
    runCPS (traverse (withFuncParam NameGen.Dependent) _fDepParams) .
    runCPS (traverse (withFuncParam NameGen.Independent) _fParams) $
    toExpression _fBody
  pure func
    { _fDepParams = depParams
    , _fParams = params
    , _fBody = body
    }

toPi ::
  MonadNaming m =>
  Pi (OldName m) (TransM m) (Expression (OldName m) (TransM m)) ->
  m (Pi (NewName m) (TransM m) (Expression (NewName m) (TransM m)))
toPi pi@Pi {..} = do
  (param, resultType) <- runCPS (withFuncParam NameGen.Dependent pParam) $ toExpression pResultType
  pure pi { pParam = param, pResultType = resultType }

toHoleActions ::
  MonadNaming m => HoleActions (OldName m) (TransM m) ->
  m (HoleActions (NewName m) (TransM m))
toHoleActions ha@HoleActions {..} = do
  run0 <- opRun
  run1 <- opRun
  let
    toHoleResult = run0 . holeResultConverted toExpression
    toScopeItem (ScopeVar getVar) = run1 $ ScopeVar <$> toGetVar getVar
    toScopeItem (ScopeTag tagG) = run1 $ ScopeTag <$> toTag tagG
    onMHoleResult = (fmap . fmap) toHoleResult
    result = onMHoleResult <$> _holeResult
    scope = _holeScope & (Lens.mapped . traverse . Lens._1) %~ toScopeItem
  pure ha { _holeScope = scope, _holeResult = result }

toHole ::
  MonadNaming m => Hole (OldName m) (TransM m) ->
  m (Hole (NewName m) (TransM m))
toHole = (holeMActions . Lens.traversed) toHoleActions

toInferred ::
  MonadNaming m => Inferred (OldName m) (TransM m) (Expression (OldName m) (TransM m)) ->
  m (Inferred (NewName m) (TransM m) (Expression (NewName m) (TransM m)))
toInferred Inferred {..} = do
  value <- toExpression _iValue
  hole <- toHole _iHole
  pure Inferred { _iValue = value, _iHole = hole, .. }

toCollapsed ::
  MonadNaming m =>
  Collapsed (OldName m) (TransM m) (Expression (OldName m) (TransM m)) ->
  m (Collapsed (NewName m) (TransM m) (Expression (NewName m) (TransM m)))
toCollapsed Collapsed {..} = do
  compact <- toGetVar _pCompact
  fullExpression <- toExpression _pFullExpression
  pure Collapsed { _pCompact = compact, _pFullExpression = fullExpression, .. }

toTag ::
  MonadNaming m => TagG (OldName m) ->
  m (TagG (NewName m))
toTag (TagG guid oldName) = do
  name <- opMakeTagName guid oldName
  pure $ TagG guid name

toGetVar ::
  MonadNaming m => GetVar (OldName m) (TransM m) ->
  m (GetVar (NewName m) (TransM m))
toGetVar getVar@GetVar{..} =
  gvName (f _gvIdentifier) getVar
  where
    f =
      case _gvVarType of
      GetParameter -> opGetParamName
      GetFieldParameter -> opMakeTagName
      GetDefinition -> opDefName

traverseToExpr ::
  (MonadNaming m, Traversable t) =>
  (t (Expression (NewName m) (TransM m)) -> b) -> t (Expression (OldName m) (TransM m)) ->
  m b
traverseToExpr cons body = cons <$> traverse toExpression body

toExpressionBody ::
  MonadNaming m =>
  ExpressionBody (OldName m) (TransM m) (Expression (OldName m) (TransM m)) ->
  m (ExpressionBody (NewName m) (TransM m) (Expression (NewName m) (TransM m)))
toExpressionBody (ExpressionApply hp body)   = traverseToExpr (ExpressionApply hp) body
toExpressionBody (ExpressionSection hp body) = traverseToExpr (ExpressionSection hp) body
toExpressionBody (ExpressionList body)       = traverseToExpr ExpressionList body
toExpressionBody (ExpressionRecord body)     = traverseToExpr ExpressionRecord body
toExpressionBody (ExpressionGetField body)   = traverseToExpr ExpressionGetField body
toExpressionBody (ExpressionLiteralInteger lit) = pure $ ExpressionLiteralInteger lit
toExpressionBody (ExpressionAtom str)           = pure $ ExpressionAtom str
--
toExpressionBody (ExpressionFunc hp func) = ExpressionFunc hp <$> toFunc func
toExpressionBody (ExpressionPi hp pi) = ExpressionPi hp <$> toPi pi
toExpressionBody (ExpressionHole hole) = ExpressionHole <$> toHole hole
toExpressionBody (ExpressionInferred inferred) = ExpressionInferred <$> toInferred inferred
toExpressionBody (ExpressionCollapsed collapsed) = ExpressionCollapsed <$> toCollapsed collapsed
toExpressionBody (ExpressionTag tag) = ExpressionTag <$> toTag tag
toExpressionBody (ExpressionGetVar getVar) = ExpressionGetVar <$> toGetVar getVar

toPayload ::
  MonadNaming m => Payload (OldName m) (TransM m) ->
  m (Payload (NewName m) (TransM m))
toPayload pl@Payload {..} = do
  inferredTypes <- traverse toExpression _plInferredTypes
  nextHole <- traverse toExpression _plNextHole
  pure pl
    { _plInferredTypes = inferredTypes
    , _plNextHole = nextHole
    }

toExpression ::
  MonadNaming m => Expression (OldName m) (TransM m) ->
  m (Expression (NewName m) (TransM m))
toExpression expr@Expression{..} = do
  body <- toExpressionBody _rExpressionBody
  pl <- toPayload _rPayload
  pure expr { _rExpressionBody = body, _rPayload = pl }

toFuncParamActions ::
  MonadNaming m => FuncParamActions (OldName m) (TransM m) ->
  m (FuncParamActions (NewName m) (TransM m))
toFuncParamActions fpa@FuncParamActions {..} =
  pure fpa { _fpGetExample = error "TODO: examples" }

withWhereItem ::
  MonadNaming m => WhereItem (OldName m) (TransM m) ->
  CPS m (WhereItem (NewName m) (TransM m))
withWhereItem item@WhereItem{..} = CPS $ \k -> do
  (name, (value, res)) <-
    runCPS (opWithWhereItemName wiGuid wiName) $
    (,) <$> toDefinitionContent wiValue <*> k
  pure (item { wiValue = value, wiName = name }, res)

toDefinitionContent ::
  MonadNaming m => DefinitionContent (OldName m) (TransM m) ->
  m (DefinitionContent (NewName m) (TransM m))
toDefinitionContent def@DefinitionContent{..} = do
  (depParams, (params, (whereItems, body))) <-
    runCPS (traverse (withFuncParam NameGen.Dependent) dDepParams) .
    runCPS (traverse (withFuncParam NameGen.Independent) dParams) .
    runCPS (traverse withWhereItem dWhereItems) $
    toExpression dBody
  pure def
    { dDepParams = depParams
    , dParams = params
    , dBody = body
    , dWhereItems = whereItems
    }

toDefinitionNewType ::
  MonadNaming m => DefinitionNewType (OldName m) (TransM m) ->
  m (DefinitionNewType (NewName m) (TransM m))
toDefinitionNewType dnt@DefinitionNewType{..} = do
  newType <- toExpression dntNewType
  pure dnt { dntNewType = newType }

toDefinitionBody ::
  MonadNaming m => DefinitionBody (OldName m) (TransM m) ->
  m (DefinitionBody (NewName m) (TransM m))
toDefinitionBody (DefinitionBodyBuiltin bi) =
  pure $ DefinitionBodyBuiltin bi
toDefinitionBody
  (DefinitionBodyExpression
   def@DefinitionExpression {..}) =
    DefinitionBodyExpression <$> do
      content <- toDefinitionContent _deContent
      mNewType <- traverse toDefinitionNewType _deMNewType
      pure def
        { _deContent = content
        , _deMNewType = mNewType
        }

toDef ::
  MonadNaming m => Definition (OldName m) (TransM m) ->
  m (Definition (NewName m) (TransM m))
toDef def@Definition {..} = do
  (name, (typ, body)) <-
    runCPS (opWithDefName _drGuid _drName) $
    (,) <$> toExpression _drType <*> toDefinitionBody _drBody
  pure def { _drName = name, _drType = typ, _drBody = body }

addToDef :: MonadA m => DefinitionU m -> DefinitionN m
addToDef = runPass1M NameGen.initial . toDef . runPass0M . toDef
