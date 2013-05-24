{-# OPTIONS -Wall -Werror #-}
module InferTests (allTests) where

import Control.Lens.Operators
import Control.Monad (void)
import Control.Monad.Trans.State (evalState)
import Data.Monoid (Monoid(..))
import InferAssert
import InferCombinators
import Lamdu.Data.Arbitrary () -- Arbitrary instance
import Lamdu.Data.Expression (Expression(..), Kind(..))
import Lamdu.Data.Expression.Infer.Conflicts (inferWithConflicts)
import Lamdu.Data.Expression.Utils (pureHole, pureSet)
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit (assertBool)
import Test.QuickCheck (Property)
import Test.QuickCheck.Property (property, rejected)
import Utils
import qualified Control.Lens as Lens
import qualified Data.Store.Guid as Guid
import qualified Lamdu.Data.Expression as Expr
import qualified Lamdu.Data.Expression.Infer as Infer
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Test.HUnit as HUnit

simpleTests :: [HUnit.Test]
simpleTests =
  [ testInfer "literal int" $ literalInteger 5
  , testInfer "simple apply" $
    holeWithInferredType (hole --> hole) $$ hole
  , testInfer "simple pi" $
    holeWithInferredType set -->
    holeWithInferredType set
  ]

applyIntToBoolFuncWithHole :: HUnit.Test
applyIntToBoolFuncWithHole =
  testInfer "apply" $
  getDef "IntToBoolFunc" $$ holeWithInferredType integerType

inferPart :: HUnit.Test
inferPart =
  testInfer "foo (xs:List ?) = 5 : xs" $
  lambda "xs" listInts $ \xs ->
  getDef ":" $$ asHole integerType $$ literalInteger 5 $$ xs
  where
    listInts = listOf (asHole integerType)

applyOnVar :: HUnit.Test
applyOnVar =
  testInfer "apply on var" $
  lambda "x" (holeWithInferredType set) $ \x ->
  getDef "IntToBoolFunc" $$
  (holeWithInferredType (hole --> integerType) $$ x)

idTest :: HUnit.Test
idTest = testInfer "id test" $ getDef "id" $$ integerType

inferFromOneArgToOther :: HUnit.Test
inferFromOneArgToOther =
  testInfer "f = \\ a b (x:Map _ _) (y:Map a b) -> if {_ x y}" $
  lambda "a" (asHole set) $ \a ->
  lambda "b" (asHole set) $ \b ->
  let mkMapType f = getDef "Map" $$ f a $$ f b in
  lambda "x" (mkMapType asHole) $ \x ->
  lambda "y" (mkMapType id) $ \y ->
  getDef "if" $$ asHole (mkMapType id) $$:
  [holeWithInferredType (getDef "Bool"), x, y]

monomorphRedex :: HUnit.Test
monomorphRedex =
  testInfer "foo = f (\\~ x -> (\\~ -> x) _) where f ~:(a:Set -> _ -> a) = _" $
  whereItem "f" (lambda "" fArgType (const hole)) $ \f ->
  f $$
  (lambda "b" (asHole set) $ \b ->
   lambda "x" (asHole b) $ \x ->
   lambda "c" (holeWithInferredType set) (\_ -> x) $$ hole)
  where
    -- (a:Set -> _[=a] -> a)
    fArgType = piType "a" set $ \a -> asHole a --> a

fOfXIsFOf5 :: HUnit.Test
fOfXIsFOf5 =
  testInfer "f x = f 5" $
  lambda "" (asHole integerType) $ \_ ->
  recurse (integerType --> hole) $$ literalInteger 5

argTypeGoesToPi :: HUnit.Test
argTypeGoesToPi =
  testInfer "arg type goes to pi" $
  holeWithInferredType (integerType --> hole) $$ literalInteger 5

idOnAnInt :: HUnit.Test
idOnAnInt =
  testInfer "id on an int" $
  getDef "id" $$ asHole integerType $$ literalInteger 5

idOnHole :: HUnit.Test
idOnHole = testInfer "id hole" $ getDef "id" $$ holeWithInferredType set

forceMono :: HUnit.Test
forceMono =
  testInfer "id (id _ _)" $
  getDef "id" $$ (getDef "id" $$ asHole set $$ holeWithInferredType set)

-- | depApply (t : Set) (rt : t -> Set) (f : (d : t) -> rt d) (x : t) = f x
depApply :: HUnit.Test
depApply =
  testInfer "dep apply" $
  lambda "t" set $ \t ->
  lambda "rt" (t --> set) $ \rt ->
  lambda "f" (piType "d" t (\d -> rt $$ d)) $ \f ->
  lambda "x" t $ \x -> f $$ x

applyFunc :: Lens.Traversal' (Expression def a) (Expression def a)
applyFunc = ExprLens.exprApply . Expr.applyFunc
applyArg :: Lens.Traversal' (Expression def a) (Expression def a)
applyArg = ExprLens.exprApply . Expr.applyArg
lamBody :: Lens.Traversal' (Expression def a) (Expression def a)
lamBody = ExprLens.exprLam . Expr.lambdaResult

resumptionTests :: [HUnit.Test]
resumptionTests =
  [ testResume "resume with pi"
    hole id (hole --> hole)
  , testResume "resume infer in apply func"
    (hole $$ hole) applyFunc (getDef "id")
  , testResume "resume infer in lambda body"
    (lambda "" hole (const hole)) lamBody (getDef "id")
  , testResume "resume infer to get param 1 of 2"
    lambdaAB lambdaABBody (getParam "a" hole)
  , testResume "resume infer to get param 2 of 2"
    lambdaAB lambdaABBody (getParam "b" hole)
  , testResume "bad a b:Set f = f a {b}"
    (lambda "a" hole $ \a ->
     lambda "b" set $ \_ ->
     lambda "f" hole $ \f -> f $$ a $$ hole)
    (lamBody . lamBody . lamBody . applyArg)
    (getParam "b" hole)
  , testCase "ref to the def on the side" $
    let
      (exprD, inferContext) =
        doInfer_ $ lambda "" hole (const hole)
      scope = exprD ^?! lamBody . Expr.ePayload . Lens.to (Infer.nScope . Infer.iPoint)
      exprR = (`evalState` inferContext) $ do
        node <- Infer.newNodeWithScope scope
        doInferM_ node getRecursiveDef
      resultR = inferResults exprR
    in assertCompareInferred resultR $ recurse (hole --> hole)
  ]
  where
    lambdaAB = lambda "a" hole . const . lambda "b" hole . const $ hole
    lambdaABBody :: Lens.Traversal' (Expression def a) (Expression def a)
    lambdaABBody = lamBody . lamBody

-- f     x    = x _ _
--   --------
--   (_ -> _)
--         ^ Set Bool here
failResumptionAddsRules :: HUnit.Test
failResumptionAddsRules =
  testCase "Resumption that adds rules and fails" .
  assertBool "Resumption should have failed" $ not success
  where
    (success, _iwc) = (`evalState` origInferContext) $
      -- TODO: Verify iwc has the right kind of conflict (or add
      -- different tests to do that)
      inferWithConflicts (doLoad resumptionValue) resumptionPoint
    resumptionValue = pureGetDef "Bool" -- <- anything but Pi
    Just pl =
      origInferred ^?
      ExprLens.exprLam . Expr.lambdaParamType .
      ExprLens.exprLam . Expr.lambdaResult .
      Expr.ePayload
    resumptionPoint = Infer.iPoint pl
    (origInferred, origInferContext) = doInfer_ origExpr
    origExpr =
      pureLambda "x" (purePi "" pureHole pureHole) $
      pureApply [pureParameterRef "x", pureHole, pureHole]

emptyRecordTest :: HUnit.Test
emptyRecordTest =
  testInfer "empty record type infer" $
  iexpr emptyRecordType pureSet emptyRecordTypeBody
  where
    emptyRecordType = ExprUtil.pureExpression emptyRecordTypeBody
    emptyRecordTypeBody =
      Expr.BodyRecord $ Expr.Record Type mempty

tagType :: Expression def ()
tagType = ExprUtil.pureExpression $ Expr.BodyLeaf Expr.TagType

recordTest :: HUnit.Test
recordTest =
  testInfer "f a x:a = {x" $
  iexpr lamA piA $
  namedLambda "a" (simple bodySet pureSet) $
  iexpr lamX piX $
  namedLambda "x" (getParamPure "a" pureSet) $
  iexpr recVal recType $
  Expr.BodyRecord $
  Expr.Record Val
  [( simple fieldTagBody tagType
   , getParamPure "x" (pureGetParam "a")
   )]
  where
    lamA =
      pureLambda "a" pureSet lamX
    lamX =
      pureLambda "x" (pureGetParam "a") recVal
    recVal =
      ExprUtil.pureExpression $
      rec Val $
      pureGetParam "x"
    fieldTagBody = ExprLens.bodyTag # fieldGuid
    rec k =
      Expr.BodyRecord . Expr.Record k .
      (:[]) . (,) (ExprLens.pureExpr # fieldTagBody)
    fieldGuid = Guid.fromString "field"
    piA =
      purePi "a" pureSet piX
    piX =
      purePi "x" (pureGetParam "a") recType
    recType =
      ExprUtil.pureExpression $
      rec Type $
      pureGetParam "a"

inferRecordValTest :: HUnit.Test
inferRecordValTest =
  testInferAllowFail "id ({:Set) <hole> infers { val" $
  getDef "id" $$ record Type [] $$ asHole (record Val [])

hunitTests :: HUnit.Test
hunitTests =
  HUnit.TestList $
  simpleTests ++
  [ applyIntToBoolFuncWithHole
  , applyOnVar
  , idTest
  , argTypeGoesToPi
  , idOnAnInt
  , idOnHole
  , inferFromOneArgToOther
  , depApply
  , forceMono
  , fOfXIsFOf5
  , monomorphRedex
  , inferPart
  , failResumptionAddsRules
  , emptyRecordTest
  , recordTest
  , inferRecordValTest
  ]
  ++ resumptionTests

inferPreservesShapeProp :: PureExprDefI t -> Property
inferPreservesShapeProp expr =
  case inferMaybe expr of
    Nothing -> property rejected
    Just (inferred, _) -> property (void inferred == expr)

qcProps :: [Test]
qcProps = [testProperty "infer preserves shape" inferPreservesShapeProp]

allTests :: [Test]
allTests = hUnitTestToTests hunitTests `mappend` qcProps
