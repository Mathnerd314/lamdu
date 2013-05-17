{-# LANGUAGE KindSignatures, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
module Lamdu.CodeEdit.Sugar.Types
  ( Definition(..), drName, drGuid, drType, drBody
  , DefinitionBody(..)
  , ListItemActions(..), itemAddNext, itemDelete
  , FuncParamActions(..), fpListItemActions, fpGetExample
  , DefinitionExpression(..), deContent, deIsTypeRedundant, deMNewType
  , DefinitionContent(..)
  , DefinitionNewType(..)
  , DefinitionBuiltin(..)
  , Actions(..)
    , giveAsArg, callWithArg, callWithNextArg
    , setToHole, replaceWithNewHole, cut, giveAsArgToOperator
  , Body(..), eHasParens
    , _BodyPi, _BodyApply, _BodySection
    , _BodyFunc, _BodyGetVar, _BodyHole
    , _BodyInferred, _BodyCollapsed
    , _BodyLiteralInteger, _BodyAtom
    , _BodyList, _BodyRecord, _BodyTag
  , Payload(..), plInferredTypes, plActions, plNextHole
  , ExpressionP(..)
    , rGuid, rBody, rPayload, rHiddenGuids, rPresugaredExpression
  , NameSource(..), NameCollision(..), Name(..), MStoredName
  , DefinitionN, DefinitionU
  , Expression, ExpressionN, ExpressionU
  , BodyN, BodyU
  , WhereItem(..)
  , ListItem(..), ListActions(..), List(..)
  , RecordField(..), rfMItemActions, rfTag, rfExpr
  , Kind(..)
  , Record(..), rKind, rFields
  , FieldList(..), flItems, flMAddFirstItem
  , GetField(..), gfRecord, gfTag
  , GetVarType(..)
  , GetVar(..), gvIdentifier, gvName, gvJumpTo, gvVarType
  , GetParams(..), gpDefGuid, gpDefName, gpJumpTo
  , Func(..), fDepParams, fParams, fBody
  , FuncParamType(..)
  , FuncParam(..), fpName, fpGuid, fpId, fpVarKind, fpHiddenLambdaGuid, fpType, fpMActions
  , TagG(..), tagName, tagGuid
  , Pi(..)
  , Section(..)
  , Hole(..), holeMActions
  , HoleResultSeed(..)
  , ScopeItem
  , Scope(..), scopeLocals, scopeGlobals, scopeTags, scopeGetParams
  , HoleActions(..), holeScope, holePaste, holeMDelete, holeResult, holeInferExprType
  , StorePoint(..)
  , HoleResult(..)
    , holeResultInferred
    , holeResultConverted
    , holeResultPick, holeResultPickPrefix
  , LiteralInteger(..)
  , Inferred(..), iValue, iHole
  , Collapsed(..), pFuncGuid, pCompact, pFullExpression
  , HasParens(..)
  , T, CT
  , PrefixAction, emptyPrefixAction
  , ExprStorePoint
  ) where

import Control.Monad.Trans.State (StateT)
import Data.Binary (Binary)
import Data.Cache (Cache)
import Data.Derive.Monoid (makeMonoid)
import Data.DeriveTH (derive)
import Data.Foldable (Foldable)
import Data.Monoid (Monoid(..))
import Data.Store.Guid (Guid)
import Data.Store.IRef (Tag)
import Data.Store.Transaction (Transaction)
import Data.Traversable (Traversable)
import Data.Typeable (Typeable)
import Lamdu.Data.Expression (Kind(..))
import Lamdu.Data.Expression.IRef (DefI)
import qualified Control.Lens.TH as LensTH
import qualified Data.List as List
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.IRef as DataIRef
import qualified Lamdu.Data.Expression.Infer as Infer

type T = Transaction
type CT m = StateT Cache (T m)

type PrefixAction m = T m ()

emptyPrefixAction :: Monad m => PrefixAction m
emptyPrefixAction = return ()

data Actions m = Actions
  { _giveAsArg :: PrefixAction m -> T m Guid
  -- Turn "x" to "x ? _" where "?" is an operator-hole.
  -- Given string is initial hole search term.
  , _giveAsArgToOperator :: T m Guid
  , _callWithNextArg :: PrefixAction m -> CT m (Maybe (T m Guid))
  , _callWithArg :: PrefixAction m -> CT m (Maybe (T m Guid))
  , _setToHole :: T m Guid
  , _replaceWithNewHole :: T m Guid
  , _cut :: T m Guid
  }
LensTH.makeLenses ''Actions

data HasParens = HaveParens | DontHaveParens

data Payload name m = Payload
  { _plInferredTypes :: [Expression name m]
  , _plActions :: Maybe (Actions m)
  , _plNextHole :: Maybe (Expression name m)
  }

newtype StorePoint t = StorePoint { unStorePoint :: DataIRef.ExpressionI t }
  deriving (Eq, Binary, Typeable)

type ExprStorePoint m = DataIRef.ExpressionM m (Maybe (StorePoint (Tag m)))

data ExpressionP name m pl = Expression
  { _rGuid :: Guid
  , _rBody :: Body name m (ExpressionP name m pl)
  , _rPayload :: pl
  , -- Guids from data model expression which were sugared out into
    -- this sugar expression.
    -- If the cursor was on them for whatever reason, it should be
    -- mapped into the sugar expression's guid.
    _rHiddenGuids :: [Guid]
  , _rPresugaredExpression :: ExprStorePoint m
  } deriving (Functor, Foldable, Traversable)

data NameSource = AutoGeneratedName | StoredName
  deriving (Show)
data NameCollision = NoCollision | Collision {-Disambiguator:-} Int
  deriving (Show)
data Name = Name
  { nNameSource :: NameSource
  , nNameCollisionSuffix :: NameCollision
  , nName :: String
  } deriving (Show)
type MStoredName = Maybe String

type Expression name m = ExpressionP name m (Payload name m)
type ExpressionN m = Expression Name m
type ExpressionU m = Expression MStoredName m

type BodyN m = Body Name m (ExpressionN m)
type BodyU m = Body MStoredName m (ExpressionU m)

data ListItemActions m = ListItemActions
  { _itemAddNext :: T m Guid
  , _itemDelete :: T m Guid
  }

data FuncParamActions name m = FuncParamActions
  { _fpListItemActions :: ListItemActions m
  , _fpGetExample :: CT m (Expression name m)
  }

data FuncParamType = FuncParameter | FuncFieldParameter

data FuncParam name m expr = FuncParam
  { -- non-unique (e.g: tag guid). Name attached here:
    _fpGuid :: Guid
     -- unique (e.g: tag expr id). WidgetId can be generated from
     -- this:
  , _fpId :: Guid
  , _fpVarKind :: FuncParamType
  , _fpName :: name
  , _fpHiddenLambdaGuid :: Maybe Guid
  , _fpType :: expr
  , _fpMActions :: Maybe (FuncParamActions name m)
  } deriving (Functor, Foldable, Traversable)

-- Multi-param Lambda
data Func name m expr = Func
  { _fDepParams :: [FuncParam name m expr]
  , _fParams :: [FuncParam name m expr]
  , _fBody :: expr
  } deriving (Functor, Foldable, Traversable)

data Pi name m expr = Pi
  { pParam :: FuncParam name m expr
  , pResultType :: expr
  } deriving (Functor, Foldable, Traversable)

-- Infix Sections include: (+), (1+), (+1), (1+2). Last is really just
-- infix application, but considered an infix section too.
data Section expr = Section
  { sectionLArg :: Maybe expr
  , sectionOp :: expr -- TODO: Always a Data.GetVariable, use a more specific type
  , sectionRArg :: Maybe expr
  } deriving (Functor, Foldable, Traversable)

data HoleResult name m = HoleResult
  { _holeResultInferred :: DataIRef.ExpressionM m (Infer.Inferred (DefI (Tag m)))
  , _holeResultConverted :: Expression name m
  , _holeResultPick :: T m (Maybe Guid)
  , _holeResultPickPrefix :: PrefixAction m
  }

data HoleResultSeed m
  = ResultSeedExpression (ExprStorePoint m)
  | ResultSeedNewTag String
  | ResultSeedNewDefinition String

type ScopeItem m a = (a, DataIRef.ExpressionM m ())

data Scope name m = Scope
  { _scopeLocals    :: [ScopeItem m (GetVar name m)]
  , _scopeGlobals   :: [ScopeItem m (GetVar name m)]
  , _scopeTags      :: [ScopeItem m (TagG name)]
  , _scopeGetParams :: [ScopeItem m (GetParams name m)]
  }

data HoleActions name m = HoleActions
  { _holeScope :: T m (Scope name m)
  , -- Infer expression "on the side" (not in the hole position),
    -- but with the hole's scope in scope.
    -- If given expression does not type check on its own, returns Nothing.
    -- (used by HoleEdit to suggest variations based on type)
    _holeInferExprType :: DataIRef.ExpressionM m () -> CT m (Maybe (DataIRef.ExpressionM m ()))
  , _holeResult :: HoleResultSeed m -> CT m (Maybe (HoleResult name m))
  , _holePaste :: Maybe (T m Guid)
  , -- TODO: holeMDelete is always Nothing, not implemented yet
    _holeMDelete :: Maybe (T m Guid)
  }

newtype Hole name m = Hole
  { _holeMActions :: Maybe (HoleActions name m)
  }

data LiteralInteger m = LiteralInteger
  { liValue :: Integer
  , liSetValue :: Maybe (Integer -> T m ())
  }

data Inferred name m expr = Inferred
  { _iValue :: expr
  , _iHole :: Hole name m
  } deriving (Functor, Foldable, Traversable)

-- TODO: New name. This is not only for polymorphic but also for eta-reduces etc
data Collapsed name m expr = Collapsed
  { _pFuncGuid :: Guid
  , _pCompact :: GetVar name m
  , _pFullExpression :: expr
  } deriving (Functor, Foldable, Traversable)

-- TODO: Do we want to store/allow-access to the implicit type params (nil's type, each cons type?)
data ListItem m expr = ListItem
  { liMActions :: Maybe (ListItemActions m)
  , liExpr :: expr
  } deriving (Functor, Foldable, Traversable)

data ListActions m = ListActions
  { addFirstItem :: T m Guid
  , replaceNil :: T m Guid
  }

data List m expr = List
  { lValues :: [ListItem m expr]
  , lMActions :: Maybe (ListActions m)
  } deriving (Functor, Foldable, Traversable)

data RecordField m expr = RecordField
  { _rfMItemActions :: Maybe (ListItemActions m)
  , _rfTag :: expr
  , _rfExpr :: expr -- field type or val
  } deriving (Functor, Foldable, Traversable)

data FieldList m expr = FieldList
  { _flItems :: [RecordField m expr]
  , _flMAddFirstItem :: Maybe (T m Guid)
  } deriving (Functor, Foldable, Traversable)

data Record m expr = Record
  { _rKind :: Kind -- record type or val
  , _rFields :: FieldList m expr
  } deriving (Functor, Foldable, Traversable)

data GetField expr = GetField
  { _gfRecord :: expr
  , _gfTag :: expr
  } deriving (Functor, Foldable, Traversable)

data GetVarType = GetDefinition | GetFieldParameter | GetParameter
  deriving (Eq, Ord)

data GetVar name m = GetVar
  { _gvIdentifier :: Guid
  , _gvName :: name
  , _gvJumpTo :: T m Guid
  , _gvVarType :: GetVarType
  }

data GetParams name m = GetParams
  { _gpDefGuid :: Guid
  , _gpDefName :: name
  , _gpJumpTo :: T m Guid
  }

data TagG name = TagG
  { _tagGuid :: Guid
  , _tagName :: name
  } deriving (Functor, Foldable, Traversable)

data Body name m expr
  = BodyApply   { _eHasParens :: HasParens, __eApply :: Expression.Apply expr }
  | BodySection { _eHasParens :: HasParens, __eSection :: Section expr }
  | BodyFunc    { _eHasParens :: HasParens, __eFunc :: Func name m expr }
  | BodyPi      { _eHasParens :: HasParens, __ePi :: Pi name m expr }
  | BodyHole    { __eHole :: Hole name m }
  | BodyInferred { __eInferred :: Inferred name m expr }
  | BodyCollapsed { __eCollapsed :: Collapsed name m expr }
  | BodyLiteralInteger { __eLit :: LiteralInteger m }
  | BodyAtom     { __eAtom :: String }
  | BodyList     { __eList :: List m expr }
  | BodyRecord   { __eRecord :: Record m expr }
  | BodyGetField { __eGetField :: GetField expr }
  | BodyTag      { __eTag :: TagG name }
  | BodyGetVar   { __eGetParam :: GetVar name m }
  | BodyGetParams { __eGetParams :: GetParams name m }
  deriving (Functor, Foldable, Traversable)

wrapParens :: HasParens -> String -> String
wrapParens HaveParens x = concat ["(", x, ")"]
wrapParens DontHaveParens x = x

instance Show expr => Show (FuncParam name m expr) where
  show fp =
    concat ["(", show (_fpGuid fp), ":", show (_fpType fp), ")"]

instance Show expr => Show (Body name m expr) where
  show BodyApply   { _eHasParens = hasParens, __eApply = Expression.Apply func arg } =
    wrapParens hasParens $ show func ++ " " ++ show arg
  show BodySection { _eHasParens = hasParens, __eSection = Section mleft op mright } =
    wrapParens hasParens $ maybe "" show mleft ++ " " ++ show op ++ maybe "" show mright
  show BodyFunc    { _eHasParens = hasParens, __eFunc = Func depParams params body } =
    wrapParens hasParens $ concat
    ["\\", parenify (showWords depParams), showWords params, " -> ", show body]
    where
      parenify "" = ""
      parenify xs = concat ["{", xs, "}"]
      showWords = unwords . map show
  show BodyPi      { _eHasParens = hasParens, __ePi = Pi paramType resultType } =
    wrapParens hasParens $ "_:" ++ show paramType ++ " -> " ++ show resultType
  show BodyHole {} = "Hole"
  show BodyInferred {} = "Inferred"
  show BodyCollapsed {} = "Collapsed"
  show BodyLiteralInteger { __eLit = LiteralInteger i _ } = show i
  show BodyAtom { __eAtom = atom } = atom
  show BodyList { __eList = List items _ } =
    concat
    [ "["
    , List.intercalate ", " $ map (show . liExpr) items
    , "]"
    ]
  show BodyRecord { __eRecord = _ } = "Record:TODO"
  show BodyGetField { __eGetField = _ } = "GetField:TODO"
  show BodyTag { __eTag = _ } = "Tag:TODO"
  show BodyGetVar {} = "GetVar:TODO"
  show BodyGetParams {} = "GetParams:TODO"

data DefinitionNewType name m = DefinitionNewType
  { dntNewType :: Expression name m
  , dntAcceptNewType :: T m ()
  }

data WhereItem name m = WhereItem
  { wiValue :: DefinitionContent name m
  , wiGuid :: Guid
  , wiName :: name
  , wiHiddenGuids :: [Guid]
  , wiActions :: Maybe (ListItemActions m)
  }

-- Common data for definitions and where-items
data DefinitionContent name m = DefinitionContent
  { dDepParams :: [FuncParam name m (Expression name m)]
  , dParams :: [FuncParam name m (Expression name m)]
  , dBody :: Expression name m
  , dWhereItems :: [WhereItem name m]
  , dAddFirstParam :: T m Guid
  , dAddInnermostWhereItem :: T m Guid
  }

data DefinitionExpression name m = DefinitionExpression
  { _deContent :: DefinitionContent name m
  , _deIsTypeRedundant :: Bool
  , _deMNewType :: Maybe (DefinitionNewType name m)
  }

data DefinitionBuiltin m = DefinitionBuiltin
  { biName :: Definition.FFIName
  -- Consider removing Maybe'ness here
  , biMSetName :: Maybe (Definition.FFIName -> T m ())
  }

data DefinitionBody name m
  = DefinitionBodyExpression (DefinitionExpression name m)
  | DefinitionBodyBuiltin (DefinitionBuiltin m)

data Definition name m = Definition
  { _drGuid :: Guid
  , _drName :: name
  , _drType :: Expression name m
  , _drBody :: DefinitionBody name m
  }

type DefinitionN = Definition Name
type DefinitionU = Definition MStoredName

derive makeMonoid ''Scope
LensTH.makePrisms ''Body
LensTH.makeLenses ''Definition
LensTH.makeLenses ''DefinitionExpression
LensTH.makeLenses ''Inferred
LensTH.makeLenses ''Collapsed
LensTH.makeLenses ''Func
LensTH.makeLenses ''FuncParam
LensTH.makeLenses ''RecordField
LensTH.makeLenses ''FieldList
LensTH.makeLenses ''Record
LensTH.makeLenses ''GetVar
LensTH.makeLenses ''GetParams
LensTH.makeLenses ''GetField
LensTH.makeLenses ''TagG
LensTH.makeLenses ''Body
LensTH.makeLenses ''ListItemActions
LensTH.makeLenses ''FuncParamActions
LensTH.makeLenses ''Payload
LensTH.makeLenses ''ExpressionP
LensTH.makeLenses ''HoleResult
LensTH.makeLenses ''Scope
LensTH.makeLenses ''HoleActions
LensTH.makeLenses ''Hole
