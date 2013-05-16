{-# LANGUAGE GeneralizedNewtypeDeriving, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable,
             PatternGuards #-}
module Lamdu.Data.Expression.Infer
  ( Inferred(..), rExpression
  , Loaded, load
  , inferLoaded
  , addRules, derefExpr
  -- TODO: Expose only ref readers for InferNode (instead of .. and TypedValue)
  , IsRestrictedPoly(..)
  , InferNode(..), TypedValue(..)
  , Error(..), ErrorDetails(..)
  , RefMap, Context, ExprRef, Scope
  , Loader(..), InferActions(..)
  , initial
  -- Used for inferring independent expressions in an inner infer context
  -- (See hole apply forms).
  , newNodeWithScope
  , newTypedNodeWithScope
  , createRefExpr
  ) where

import Control.Applicative (Applicative(..), (<$>))
import Control.DeepSeq (NFData(..))
import Control.Lens (LensLike', (%=), (.=), (^.), (^?), (+=), (%~), (&), (<>~))
import Control.Monad ((<=<), guard, unless, void, when)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Either (EitherT(..))
import Control.Monad.Trans.State (StateT(..), State, runState)
import Control.Monad.Trans.State.Utils (toStateT)
import Control.Monad.Trans.Writer (Writer)
import Control.MonadA (MonadA)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Derive.Binary (makeBinary)
import Data.Derive.NFData (makeNFData)
import Data.DeriveTH (derive)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Functor.Identity (Identity(..))
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Maybe (isJust, mapMaybe, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.Traversable (traverse)
import Data.Typeable (Typeable)
import Lamdu.Data.Expression.IRef (DefI)
import Lamdu.Data.Expression.Infer.Rules (Rule(..))
import Lamdu.Data.Expression.Infer.Types
import qualified Control.Lens as Lens
import qualified Control.Lens.TH as LensTH
import qualified Control.Lens.Utils as LensUtils
import qualified Control.Monad.Trans.Either as Either
import qualified Control.Monad.Trans.State as State
import qualified Data.Foldable as Foldable
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Infer.Rules as Rules
import qualified Lamdu.Data.Expression.Utils as ExprUtil

mkOrigin :: State Origin Origin
mkOrigin = do
  r <- State.get
  State.modify (+1)
  return r

newtype RuleRef = RuleRef { unRuleRef :: Int }

instance Show RuleRef where
  show = ('E' :) . show . unRuleRef

-- Initial Pass:
-- Get Definitions' types expand.
-- Use expression's structures except for Apply.
--   (because an Apply can result in something else
--    but for example an Int or Lambda stays the same)
-- Add SimpleType, Union, LambdaOrPi, LambdaBodyType, Apply rules
-- Param types of Lambdas and Pis are of type Set
-- Pi result type is of type Set

-- When recursing on an expression, we remember the parent expression origins,
-- And we make sure not to add a sub-expression with a parent origin (that's a recursive structure).

data RefData def = RefData
  { _rExpression :: RefExpression def
  , _rRules :: [RuleRef] -- Rule id
  }

--------------
--- RefMap:
data RefMap a = RefMap
  { _refs :: IntMap a
  , _nextRef :: Int
  }
LensTH.makeLenses ''RefData
LensTH.makeLenses ''RefMap

emptyRefMap :: RefMap a
emptyRefMap =
  RefMap
  { _refs = mempty
  , _nextRef = 0
  }

{-# INLINE createRef #-}
createRef :: a -> State (RefMap a) Int
createRef val = do
  key <- Lens.use nextRef
  nextRef += 1
  refs . Lens.at key .= Just val
  return key

{-# INLINE refsAt #-}
refsAt :: Functor f => Int -> LensLike' f (RefMap a) a
refsAt k =
  refs . Lens.at k .
  LensUtils._fromJust (unwords ["intMapMod: key", show k, "not in map"])
-------------- InferActions

data ErrorDetails def
  = MismatchIn
    (Expression.Expression def ())
    (Expression.Expression def ())
  | InfiniteExpression (Expression.Expression def ())
  deriving (Show, Eq, Ord)

instance Functor ErrorDetails where
  fmap f (MismatchIn x y) =
    on MismatchIn (ExprUtil.expressionDef %~ f) x y
  fmap f (InfiniteExpression x) =
    InfiniteExpression $ x & ExprUtil.expressionDef %~ f

data Error def = Error
  { errRef :: ExprRef
  , errMismatch ::
    ( Expression.Expression def ()
    , Expression.Expression def ()
    )
  , errDetails :: ErrorDetails def
  } deriving (Show, Eq, Ord)

instance Functor Error where
  fmap f (Error ref mis details) =
    Error ref
    (mis & Lens.both . ExprUtil.expressionDef %~ f)
    (f <$> details)

newtype InferActions def m = InferActions
  { reportError :: Error def -> m ()
  }

--------------

data Context def = Context
  { _exprMap :: RefMap (RefData def)
  , _nextOrigin :: Int
  , _ruleMap :: RefMap (Rule def ExprRef)
  } deriving (Typeable)

data InferState def m = InferState
  { _sContext :: Context def
  , _sBfsNextLayer :: IntSet
  , _sBfsCurLayer :: IntSet
  , _sActions :: InferActions def m
  }
LensTH.makeLenses ''Context
LensTH.makeLenses ''InferState

fmap concat . sequence $
  derive
  <$> [makeBinary, makeNFData]
  <*> [''Context, ''ErrorDetails, ''Error, ''RuleRef, ''RefData, ''RefMap]

-- ExprRefMap:

toRefExpression :: Expression.Expression def () -> State Origin (RefExpression def)
toRefExpression =
  traverse . const $
  RefExprPayload mempty (Monoid.Any False) <$> mkOrigin

createRefExpr :: State (Context def) ExprRef
createRefExpr = do
  holeRefExpr <- Lens.zoom nextOrigin $ toRefExpression ExprUtil.pureHole
  fmap ExprRef . Lens.zoom exprMap . createRef $ RefData holeRefExpr mempty

{-# INLINE exprRefsAt #-}
exprRefsAt :: Functor f => ExprRef -> LensLike' f (Context def) (RefData def)
exprRefsAt k = exprMap . refsAt (unExprRef k)

-- RuleRefMap

createRuleRef :: Rule def ExprRef -> State (Context def) RuleRef
createRuleRef = fmap RuleRef . Lens.zoom ruleMap . createRef

{-# INLINE ruleRefsAt #-}
ruleRefsAt :: Functor f => RuleRef -> LensLike' f (Context def) (Rule def ExprRef)
ruleRefsAt k = ruleMap . refsAt (unRuleRef k)

-------------

-- TODO: createTypeVal should use newNode, not vice versa.
-- For use in loading phase only!
-- We don't create additional Refs afterwards!
createTypedVal :: State (Context def) TypedValue
createTypedVal = TypedValue <$> createRefExpr <*> createRefExpr

newNodeWithScope :: Scope def -> State (Context def) (InferNode def)
newNodeWithScope scope = (`InferNode` scope) <$> createTypedVal

newTypedNodeWithScope ::
  Scope def -> ExprRef -> State (Context def) (InferNode def)
newTypedNodeWithScope scope typ =
  (`InferNode` scope) . (`TypedValue` typ) <$> createRefExpr

initial :: Ord def => Maybe def -> (Context def, InferNode def)
initial mRecursiveDefI =
  (context, res)
  where
    (res, context) =
      (`runState` emptyContext) $ do
        rootTv <- createTypedVal
        let
          scope =
            case mRecursiveDefI of
            Nothing -> mempty
            Just recursiveDefI ->
              Map.singleton (Expression.DefinitionRef recursiveDefI) (tvType rootTv)
        return $ InferNode rootTv scope
    emptyContext =
      Context
      { _exprMap = emptyRefMap
      , _nextOrigin = 0
      , _ruleMap = emptyRefMap
      }

--- InferT:

newtype InferT def m a =
  InferT { unInferT :: StateT (InferState def m) m a }
  deriving (Functor, Applicative, Monad)

askActions :: MonadA m => InferT def m (InferActions def m)
askActions = InferT $ Lens.use sActions

liftState :: Monad m => StateT (InferState def m) m a -> InferT def m a
liftState = InferT

{-# SPECIALIZE liftState :: StateT (InferState def Maybe) Maybe a -> InferT def Maybe a #-}
{-# SPECIALIZE liftState :: Monoid w => StateT (InferState def (Writer w)) (Writer w) a -> InferT def (Writer w) a #-}

instance MonadTrans (InferT def) where
  lift = liftState . lift

derefExpr ::
  Expression.Expression def (InferNode def, a) -> Context def ->
  Expression.Expression def (Inferred def, a)
derefExpr expr context =
  derefNode <$> expr
  where
    derefNode (inferNode, s) =
      ( Inferred
        { iValue = deref . tvVal $ nRefs inferNode
        , iType = deref . tvType $ nRefs inferNode
        , iScope =
          Map.fromList . mapMaybe onScopeElement . Map.toList $ nScope inferNode
        , iPoint = inferNode
        }
      , s
      )
    onScopeElement (Expression.ParameterRef guid, ref) = Just (guid, deref ref)
    onScopeElement _ = Nothing
    toIsRestrictedPoly False = UnrestrictedPoly
    toIsRestrictedPoly True = RestrictedPoly
    deref ref =
      toIsRestrictedPoly . Monoid.getAny . Lens.view rplRestrictedPoly <$>
      context ^. exprRefsAt ref . rExpression

getRefExpr :: MonadA m => ExprRef -> InferT def m (RefExpression def)
getRefExpr ref = liftState $ Lens.use (sContext . exprRefsAt ref . rExpression)

{-# SPECIALIZE getRefExpr :: ExprRef -> InferT (DefI t) Maybe (RefExpression (DefI t)) #-}
{-# SPECIALIZE getRefExpr :: Monoid w => ExprRef -> InferT (DefI t) (Writer w) (RefExpression (DefI t)) #-}

executeRules :: (Eq def, MonadA m) => InferT def m ()
executeRules = do
  curLayer <- liftState $ Lens.use sBfsNextLayer
  liftState $ sBfsCurLayer .= curLayer
  liftState $ sBfsNextLayer .= IntSet.empty
  unless (IntSet.null curLayer) $ do
    traverse_ processRule $ IntSet.toList curLayer
    executeRules
  where
    processRule key = do
      liftState $ sBfsCurLayer . Lens.contains key .= False
      ruleRefs <- liftState $ Lens.use (sContext . ruleRefsAt (RuleRef key))
      ruleExprs <- traverse getRefExpr ruleRefs
      traverse_ (uncurry setRefExpr) $ Rules.runRule ruleExprs

{-# SPECIALIZE executeRules :: InferT (DefI t) Maybe () #-}
{-# SPECIALIZE executeRules :: Monoid w => InferT (DefI t) (Writer w) () #-}

execInferT ::
  (MonadA m, Eq def) => InferActions def m ->
  InferT def m a -> StateT (Context def) m a
execInferT actions act = do
  inferState <- State.gets mkInferState
  (res, newState) <-
    lift . (`runStateT` inferState) . unInferT $ do
      res <- act
      executeRules
      return res
  State.put $ newState ^. sContext
  return res
  where
    mkInferState ctx = InferState ctx mempty mempty actions

{-# SPECIALIZE
  execInferT ::
    InferActions (DefI t) Maybe -> InferT (DefI t) Maybe a ->
    StateT (Context (DefI t)) Maybe a
  #-}

{-# SPECIALIZE
  execInferT ::
    Monoid w =>
    InferActions (DefI t) (Writer w) -> InferT (DefI t) (Writer w) a ->
    StateT (Context (DefI t)) (Writer w) a
  #-}

newtype Loader def m = Loader
  { loadPureDefinitionType :: def -> m (Expression.Expression def ())
  }

-- This is because platform's Either's MonadA instance sucks
runEither :: EitherT l Identity a -> Either l a
runEither = runIdentity . runEitherT

guardEither :: l -> Bool -> EitherT l Identity ()
guardEither err False = Either.left err
guardEither _ True = return ()

originRepeat :: RefExpression def -> Bool
originRepeat =
  go Set.empty
  where
    go forbidden (Expression.Expression body pl)
      | Set.member g forbidden = True
      | otherwise =
        Foldable.any (go (Set.insert g forbidden)) body
      where
        g = Lens.view rplOrigin pl

-- Merge two expressions:
-- If they do not match, return Nothing.
-- Holes match with anything, expand to the other expr.
-- Param guids and Origins come from the first expression.
-- If origins repeat, fail.
mergeExprs ::
  Eq def =>
  RefExpression def ->
  RefExpression def ->
  Either (ErrorDetails def) (RefExpression def)
mergeExprs p0 p1 =
  runEither $ do
    result <- ExprUtil.matchExpression onMatch onMismatch p0 p1
    guardEither (InfiniteExpression (void result)) . not $ originRepeat result
    return result
  where
    src `mergePayloadInto` dest =
      mappendLens rplRestrictedPoly src .
      mappendLens rplSubstitutedArgs src $
      dest
    mappendLens lens src =
      Lens.cloneLens lens <>~ src ^. Lens.cloneLens lens
    onMatch x y = return $ y `mergePayloadInto` x
    onMismatch (Expression.Expression (Expression.BodyLeaf Expression.Hole) s0) e1 =
      return $ (s0 `mergePayloadInto`) <$> e1
    onMismatch e0 (Expression.Expression (Expression.BodyLeaf Expression.Hole) s1) =
      return $ (s1 `mergePayloadInto`) <$> e0
    onMismatch e0 e1 =
      Either.left $ MismatchIn (void e0) (void e1)

touch :: MonadA m => ExprRef -> InferT def m ()
touch ref =
  liftState $ do
    nodeRules <- Lens.use (sContext . exprRefsAt ref . rRules)
    curLayer <- Lens.use sBfsCurLayer
    sBfsNextLayer %=
      ( mappend . IntSet.fromList
      . filter (not . (`IntSet.member` curLayer))
      . map unRuleRef
      ) nodeRules

{-# SPECIALIZE touch :: ExprRef -> InferT (DefI t) Maybe () #-}
{-# SPECIALIZE touch :: Monoid w => ExprRef -> InferT (DefI t) (Writer w) () #-}

setRefExpr :: (Eq def, MonadA m) => ExprRef -> RefExpression def -> InferT def m ()
setRefExpr ref newExpr = do
  curExpr <- liftState $ Lens.use (sContext . exprRefsAt ref . rExpression)
  case mergeExprs curExpr newExpr of
    Right mergedExpr -> do
      let
        isChange = not $ equiv mergedExpr curExpr
        isHole =
          Lens.notNullOf
          (Expression.eBody . Expression._BodyLeaf . Expression._Hole)
          mergedExpr
      when isChange $ touch ref
      when (isChange || isHole) $
        liftState $ sContext . exprRefsAt ref . rExpression .= mergedExpr
    Left details -> do
      report <- fmap reportError askActions
      lift $ report Error
        { errRef = ref
        , errMismatch = (void curExpr, void newExpr)
        , errDetails = details
        }
  where
    equiv x y =
      isJust $
      ExprUtil.matchExpression comparePl ((const . const) Nothing) x y
    comparePl x y =
      guard $
      (x ^. rplSubstitutedArgs) == (y ^. rplSubstitutedArgs) &&
      (x ^. rplRestrictedPoly) == (y ^. rplRestrictedPoly)

{-# SPECIALIZE setRefExpr :: ExprRef -> RefExpression (DefI t) -> InferT (DefI t) Maybe () #-}
{-# SPECIALIZE setRefExpr :: Monoid w => ExprRef -> RefExpression (DefI t) -> InferT (DefI t) (Writer w) () #-}

liftOriginState :: MonadA m => State Origin a -> InferT def m a
liftOriginState = liftState . Lens.zoom (sContext . nextOrigin) . toStateT

liftContextState :: MonadA m => State (Context def) a -> InferT def m a
liftContextState = liftState . Lens.zoom sContext . toStateT

exprIntoContext ::
  (MonadA m, Ord def) => Scope def ->
  Loaded def a ->
  InferT def m (Expression.Expression def (InferNode def, a))
exprIntoContext rootScope (Loaded rootExpr defTypes) = do
  defTypesRefs <-
    traverse defTypeIntoContext $
    Map.mapKeys Expression.DefinitionRef defTypes
  -- mappend prefers left, so it is critical we put rootScope
  -- first. defTypesRefs may contain the loaded recursive defI because
  -- upon resumption, we load without giving the root defI, so its
  -- type does get (unnecessarily) loaded.
  go (rootScope `mappend` defTypesRefs) =<<
    liftContextState
    (traverse addTypedVal rootExpr)
  where
    defTypeIntoContext defType = do
      ref <- liftContextState createRefExpr
      setRefExpr ref =<< liftOriginState (toRefExpression defType)
      return ref
    addTypedVal x = fmap ((,) x) createTypedVal
    go scope (Expression.Expression body (s, createdTV)) = do
      inferNode <- toInferNode scope (void <$> body) createdTV
      newBody <-
        case body of
        Expression.BodyLam (Expression.Lambda k paramGuid paramType result) -> do
          paramTypeDone <- go scope paramType
          let
            paramTypeRef = tvVal . nRefs . fst $ paramTypeDone ^. Expression.ePayload
            newScope = Map.insert (Expression.ParameterRef paramGuid) paramTypeRef scope
          resultDone <- go newScope result
          return $ ExprUtil.makeLam k paramGuid paramTypeDone resultDone
        _ -> traverse (go scope) body
      return $ Expression.Expression newBody (inferNode, s)
    toInferNode scope body tv = do
      let
        typedValue =
          tv
          { tvType =
              fromMaybe (tvType tv) $
              body ^?
                Expression._BodyLeaf . Expression._GetVariable .
                Lens.folding (`Map.lookup` scope)
          }
      return $ InferNode typedValue scope

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

data Loaded def a = Loaded
  { _lExpr :: Expression.Expression def a
  , _lDefTypes :: Map def (Expression.Expression def ())
  } deriving (Typeable, Functor)
-- Requires Ord instance for def, cannot derive
instance (Binary a, Binary def, Ord def) => Binary (Loaded def a) where
  get = Loaded <$> get <*> get
  put (Loaded a b) = put a >> put b

load ::
  (MonadA m, Ord def) =>
  Loader def m -> Maybe def -> Expression.Expression def a -> m (Loaded def a)
load loader mRecursiveDef expr =
  fmap (Loaded expr) loadDefTypes
  where
    loadDefTypes =
      fmap Map.fromList .
      traverse loadType . ordNub $
      Lens.toListOf
      ( Lens.folding ExprUtil.subExpressions
      . Expression.eBody . ExprUtil.bodyDefinitionRef
      . Lens.filtered ((/= mRecursiveDef) . Just)
      ) expr
    loadType defI = fmap ((,) defI) $ loadPureDefinitionType loader defI

addRule :: Rule def ExprRef -> State (InferState def m) ()
addRule rule = do
  ruleRef <- makeRule
  traverse_ (addRuleId ruleRef) $ Foldable.toList rule
  sBfsNextLayer . Lens.contains (unRuleRef ruleRef) .= True
  where
    makeRule = Lens.zoom sContext $ createRuleRef rule
    addRuleId ruleRef ref = sContext . exprRefsAt ref . rRules %= (ruleRef :)

addRules ::
  (Eq def, MonadA m) => InferActions def m ->
  [Expression.Expression def (InferNode def)] ->
  StateT (Context def) m ()
addRules actions exprs =
  execInferT actions . liftState . toStateT $
  traverse_ addRule . concat =<<
  (Lens.zoom (sContext . nextOrigin) .
   traverse Rules.makeForNode . (map . fmap) nRefs) exprs

inferLoaded ::
  (Ord def, MonadA m) =>
  InferActions def m -> Loaded def a ->
  InferNode def ->
  StateT (Context def) m (Expression.Expression def (Inferred def, a))
inferLoaded actions loadedExpr node =
  State.gets . derefExpr <=<
  execInferT actions $ do
    expr <- exprIntoContext (nScope node) loadedExpr
    liftState . toStateT $ do
      let
        addUnionRules f =
          traverse_ addRule $ on Rules.union (f . nRefs) node . fst $ expr ^. Expression.ePayload
      addUnionRules tvVal
      addUnionRules tvType
      rules <-
        Lens.zoom (sContext . nextOrigin) .
        Rules.makeForAll $ nRefs . fst <$> expr
      traverse_ addRule rules
    return expr

{-# SPECIALIZE
  inferLoaded ::
    InferActions (DefI t) Maybe -> Loaded (DefI t) a ->
    InferNode (DefI t) ->
    StateT (Context (DefI t)) Maybe (DataIRef.Expression t (Inferred (DefI t), a))
  #-}
{-# SPECIALIZE
  inferLoaded ::
    Monoid w => InferActions (DefI t) (Writer w) -> Loaded (DefI t) a ->
    InferNode (DefI t) ->
    StateT (Context (DefI t)) (Writer w) (DataIRef.Expression t (Inferred (DefI t), a))
  #-}
