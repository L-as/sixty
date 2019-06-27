{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language PackageImports #-}
module Inlining where

import Protolude hiding (Type, IntMap, evaluate, empty)

import "this" Data.IntMap (IntMap)
import Data.IntSequence (IntSeq)
import qualified Data.IntSequence as IntSeq
import Index
import qualified Meta
import Monad
import Name (Name)
import qualified Name
import Plicity
import qualified "this" Data.IntMap as IntMap
import qualified Readback
import qualified Syntax
import Telescope (Telescope)
import qualified Telescope
import Var (Var)
import qualified Var

inlineDefinition :: Syntax.Definition -> M Syntax.Definition
inlineDefinition def =
  case def of
    Syntax.TypeDeclaration type_ ->
      Syntax.TypeDeclaration <$> inlineTerm empty type_

    Syntax.ConstantDefinition term ->
      Syntax.ConstantDefinition <$> inlineTerm empty term

    Syntax.DataDefinition tele ->
      Syntax.DataDefinition <$> inlineDataDefinition empty tele

inlineDataDefinition
  :: Environment v
  -> Telescope Syntax.Type Syntax.ConstructorDefinitions v
  -> M (Telescope Syntax.Type Syntax.ConstructorDefinitions v)
inlineDataDefinition env tele =
  case tele of
    Telescope.Empty (Syntax.ConstructorDefinitions constrDefs) -> do
      constrDefs' <- forM constrDefs $ \(constr, type_) -> do
        type' <- inlineTerm env type_
        pure (constr, type')
      pure $ Telescope.Empty $ Syntax.ConstructorDefinitions constrDefs'

    Telescope.Extend name type_ plicity tele' -> do
      type' <- inlineTerm env type_
      (env', _) <- extend env
      tele'' <- inlineDataDefinition env' tele'
      pure $ Telescope.Extend name type' plicity tele''

inlineTerm :: Environment v -> Syntax.Term v -> M (Syntax.Term v)
inlineTerm env term = do
  value <- evaluate env term
  pure $ readback Readback.Environment { vars = vars env } value

data Value
  = Var !Var
  | Global !Name.Qualified
  | Con !Name.QualifiedConstructor
  | Meta !Meta.Index
  | Let !Name !Var !Value !Type !Value
  | Pi !Name !Var !Type !Plicity !Type
  | Fun !Type !Type
  | Lam !Name !Var !Type !Plicity !Value
  | App !Value !Plicity !Value
  deriving Show

type Type = Value

data Environment v = Environment
  { vars :: IntSeq Var
  , values :: IntMap Var Value
  }

empty :: Environment Void
empty = Environment
  { vars = mempty
  , values = mempty
  }

extend :: Environment v -> M (Environment (Succ v), Var)
extend env = do
  var <- freshVar
  let
    env' =
      env
        { vars = vars env IntSeq.:> var
        }
  pure (env', var)

extendValue :: Environment v -> Value -> M (Environment (Succ v))
extendValue env value = do
  (env', var) <- extend env
  pure $
    env'
      { values = IntMap.insert var value $ values env'
      }

evaluate :: Environment v -> Syntax.Term v -> M Value
evaluate env term =
  case term of
    Syntax.Var (Index index) -> do
      let
        var =
          IntSeq.index (vars env) (IntSeq.length (vars env) - index - 1)
      case IntMap.lookup var (values env) of
        Nothing ->
          pure $ Var var

        Just value ->
          pure value

    Syntax.Global global ->
      pure $ Global global

    Syntax.Con con ->
      pure $ Con con

    Syntax.Meta meta ->
      pure $ Meta meta

    Syntax.Let name term' type_ body
      | duplicable term' -> do
        value <- evaluate env term'
        env' <- extendValue env value
        evaluate env' body

      | otherwise -> do
        (env', var) <- extend env
        Let name var <$>
          evaluate env term' <*>
          evaluate env type_ <*>
          evaluate env' body

    Syntax.Pi name source plicity domain -> do
      (env', var) <- extend env
      Pi name var <$>
        evaluate env source <*>
        pure plicity <*>
        evaluate env' domain

    Syntax.Fun source domain ->
      Fun <$> evaluate env source <*> evaluate env domain

    Syntax.Lam name type_ plicity body -> do
      (env', var) <- extend env
      Lam name var <$>
        evaluate env type_ <*>
        pure plicity <*>
        evaluate env' body

    Syntax.App fun plicity arg ->
      App <$> evaluate env fun <*> pure plicity <*> evaluate env arg

readback :: Readback.Environment v -> Value -> Syntax.Term v
readback env value =
  case value of
    Var var ->
      case Readback.lookupVarIndex var env of
        Just i ->
          Syntax.Var i

        Nothing ->
          panic "Substitution.readback: scoping error"

    Global global ->
      Syntax.Global global

    Con con ->
      Syntax.Con con

    Meta meta ->
      Syntax.Meta meta

    Let name var term type_ body -> do
      let
        env' =
          Readback.extendVar env var
      Syntax.Let name (readback env term) (readback env type_) (readback env' body)

    Pi name var source plicity domain -> do
      let
        env' =
          Readback.extendVar env var
      Syntax.Pi name (readback env source) plicity (readback env' domain)

    Fun source domain ->
      Syntax.Fun (readback env source) (readback env domain)

    Lam name var type_ plicity body -> do
      let
        env' =
          Readback.extendVar env var
      Syntax.Lam name (readback env type_) plicity (readback env' body)

    App fun plicity arg ->
      Syntax.App (readback env fun) plicity (readback env arg)

duplicable :: Syntax.Term v -> Bool
duplicable term =
  case term of
    Syntax.Var {} ->
      True

    Syntax.Global {} ->
      True

    Syntax.Con {} ->
      True

    Syntax.Meta {} ->
      True

    _ ->
      False