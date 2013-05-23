{-# LANGUAGE TemplateHaskell #-}
module Lamdu.CodeEdit.FFI (Env(..), table) where

import Control.Lens ((^?))
import Data.Binary (Binary(..))
import Data.Derive.Binary (makeBinary)
import Data.DeriveTH (derive)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Control.Lens as Lens
import qualified Data.Map as Map
import qualified Lamdu.Data.Definition as Definition
import qualified Lamdu.Data.Expression as Expression
import qualified Lamdu.Data.Expression.Utils as ExprUtil
import qualified Lamdu.Data.Expression.Lens as ExprLens
import qualified Lamdu.Data.Expression.IRef as DataIRef

data Env t = Env
  { trueDef :: DataIRef.DefI t
  , falseDef :: DataIRef.DefI t
  }
derive makeBinary ''Env

class FromExpr a where
  fromExpr :: Env t -> DataIRef.Expression t () -> a

class ToExpr a where
  toExpr :: Env t -> a -> [DataIRef.Expression t ()] -> DataIRef.Expression t ()

instance FromExpr Integer where
  fromExpr _ e =
    fromMaybe (error "Expecting normalized Integer expression!") $
    e ^? Expression.eBody . ExprLens.bodyLiteralInteger

instance ToExpr Integer where
  toExpr _ x [] = ExprUtil.pureLiteralInteger x
  toExpr _ _ _ = error "Integer applied as a function"

instance (FromExpr a, ToExpr b) => ToExpr (a -> b) where
  toExpr _ _ [] = error "Expecting more arguments"
  toExpr env f (x:xs) = (toExpr env . f . fromExpr env) x xs

instance ToExpr Bool where
  toExpr env b [] =
    ExprUtil.pureExpression . Lens.review ExprLens.bodyDefinitionRef $
    (if b then trueDef else falseDef) env
  toExpr _ _ _ = error "Bool applied as a function"

instance FromExpr Bool where
  fromExpr env expr =
    case expr ^? Expression.eBody . ExprLens.bodyDefinitionRef of
    Just defRef
      | defRef == trueDef env -> True
      | defRef == falseDef env -> False
    _ -> error "Expected a normalized bool expression!"

table :: Env t -> Map Definition.FFIName ([DataIRef.Expression t ()] -> DataIRef.Expression t ())
table env =
  Map.fromList
  [ prelude "==" ((==) :: Integer -> Integer -> Bool)
  , prelude "+" ((+) :: Integer -> Integer -> Integer)
  , prelude "-" ((-) :: Integer -> Integer -> Integer)
  , prelude "*" ((*) :: Integer -> Integer -> Integer)
  ]
  where
    prelude name val =
      (Definition.FFIName ["Prelude"] name, toExpr env val)
