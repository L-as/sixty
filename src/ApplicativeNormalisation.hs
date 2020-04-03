{-# language OverloadedStrings #-}
module ApplicativeNormalisation where

import Protolude hiding (evaluate, Type)

import qualified Applicative.Syntax as Applicative
import qualified Binding
import qualified Builtin
import ClosureConverted.Context (Context)
import qualified ClosureConverted.Context as Context
import qualified ClosureConverted.Domain
import qualified ClosureConverted.Evaluation
import qualified ClosureConverted.Readback
import qualified ClosureConverted.Syntax as ClosureConverted
import qualified ClosureConverted.TypeOf
import Data.OrderedHashMap (OrderedHashMap)
import qualified Data.OrderedHashMap as OrderedHashMap
import qualified Environment
import Literal (Literal)
import Monad
import Name (Name)
import qualified Name
import Syntax.Telescope (Telescope)
import qualified Syntax.Telescope as Telescope
import Var (Var)

data Value
  = Operand !Operand
  | Con !Name.QualifiedConstructor [Operand] [Operand]
  | Let !Name !Var !Value !TypeOperand Value
  | Function [(Name, Var, Type)] !Type
  | Apply !Name.Lifted [Operand]
  | Pi !Name !Var !Type !Type
  | Closure !Name.Lifted [Operand]
  | ApplyClosure !Operand [Operand]
  | Case !Operand !Branches !(Maybe Value)
  deriving Show

type Type = Value

data Operand
  = Var !Var
  | Global !Name.Lifted
  | Lit !Literal
  deriving Show

type TypeOperand = Operand

data Branches
  = ConstructorBranches !Name.Qualified (OrderedHashMap Name.Constructor ([(Name, Var, Type)], Value))
  | LiteralBranches (OrderedHashMap Literal Value)
  deriving Show

-------------------------------------------------------------------------------

withOperand
  :: Context v
  -> ClosureConverted.Term v
  -> (Context v -> Operand -> M Value)
  -> M Value
withOperand context term k =
  case term of
    ClosureConverted.Var index ->
      k context $ Var $ Environment.lookupIndexVar index $ Context.toEnvironment context

    ClosureConverted.Global global ->
      k context $ Global global

    ClosureConverted.Lit lit ->
      k context $ Lit lit

    _ -> do
      type_ <- typeOf context term
      withOperand context type_ $ \context' typeOperand -> do
        value <- evaluate context' term
        typeValue <- ClosureConverted.Evaluation.evaluate (Context.toEnvironment context') type_
        (context'', var) <- Context.extendUnindexed context typeValue
        resultValue <- k context'' $ Var var
        pure $ Let "x" var value typeOperand resultValue

withOperands
  :: Context v
  -> [ClosureConverted.Term v]
  -> (Context v -> [Operand] -> M Value)
  -> M Value
withOperands context terms k =
  case terms of
    [] ->
      k context []

    term:terms' ->
      withOperand context term $ \context' operand ->
      withOperands context' terms' $ \context'' operands ->
          k context'' $ operand : operands

typeOf
  :: Context v
  -> ClosureConverted.Term v
  -> M (ClosureConverted.Type v)
typeOf context term = do
  let
    env =
      Context.toEnvironment context
  value <- ClosureConverted.Evaluation.evaluate env term
  typeValue <- ClosureConverted.TypeOf.typeOf context value
  ClosureConverted.Readback.readback env typeValue

-------------------------------------------------------------------------------

evaluate :: Context v -> ClosureConverted.Term v -> M Value
evaluate context term =
  case term of
    ClosureConverted.Var index ->
      pure $ Operand $ Var $ Environment.lookupIndexVar index $ Context.toEnvironment context

    ClosureConverted.Global global ->
      pure $ Operand $ Global global

    ClosureConverted.Con con params args ->
      withOperands context params $ \context' params' ->
      withOperands context' args $ \_ args' ->
        pure $ Con con params' args'

    ClosureConverted.Lit lit ->
      pure $ Operand $ Lit lit

    ClosureConverted.Let name term' type_ body -> do
      typeValue <- ClosureConverted.Evaluation.evaluate (Context.toEnvironment context) type_
      withOperand context type_ $ \context' type' -> do
        term'' <- evaluate context' term'
        (context'', var) <- Context.extend context' typeValue
        body' <- evaluate context'' body
        pure $ Let name var term'' type' body'

    ClosureConverted.Function tele -> do
      uncurry Function <$> evaluateTelescope (Context.emptyFrom context) tele

    ClosureConverted.Apply global args ->
      withOperands context args $ \_ args' ->
        pure $ Apply global args'

    ClosureConverted.Pi name domain target -> do
      domain' <- evaluate context domain
      (context', var) <- Context.extend context $ ClosureConverted.Domain.global $ Name.Lifted Builtin.TypeName 0
      Pi name var domain' <$> evaluate context' target

    ClosureConverted.Closure global args ->
      withOperands context args $ \_ args' ->
        pure $ Closure global args'

    ClosureConverted.ApplyClosure term' args ->
      withOperand context term' $ \context' operand ->
      withOperands context' args $ \_ args' ->
        pure $ ApplyClosure operand args'

    ClosureConverted.Case scrutinee branches defaultBranch ->
      withOperand context scrutinee $ \context' scrutinee' ->
      Case scrutinee' <$>
        evaluateBranches context' branches <*>
        mapM (evaluate context') defaultBranch

evaluateBranches
  :: Context v
  -> ClosureConverted.Branches v
  -> M Branches
evaluateBranches context branches =
  case branches of
    ClosureConverted.ConstructorBranches constructorTypeName constructorBranches ->
      ConstructorBranches constructorTypeName <$> OrderedHashMap.mapMUnordered (evaluateTelescope context) constructorBranches

    ClosureConverted.LiteralBranches literalBranches ->
      LiteralBranches <$> OrderedHashMap.mapMUnordered (evaluate context) literalBranches

evaluateTelescope
  :: Context v
  -> Telescope ClosureConverted.Type ClosureConverted.Term v
  -> M ([(Name, Var, Type)], Value)
evaluateTelescope context tele =
  case tele of
    Telescope.Empty body -> do
      body' <- evaluate context body
      pure ([], body')

    Telescope.Extend binding type_ _plicity tele' -> do
      typeValue <- ClosureConverted.Evaluation.evaluate (Context.toEnvironment context) type_
      type' <- evaluate context type_
      (context', var) <- Context.extend context typeValue
      (names, body) <- evaluateTelescope context' tele'
      pure ((Binding.toName binding, var, type'):names, body)

-------------------------------------------------------------------------------

-- readback :: ClosureConverted.Domain.Environment v -> Value -> M (ClosureConverted.Term v)
-- readback env value =
--   case value of
--     Domain.Neutral head args -> do
--       args' <- mapM (readback env) args
--       case head of
--         Domain.Var var -> do
--           let
--             term =
--               Syntax.Var $ fromMaybe (panic "ClosureConverted.Readback var") $ Environment.lookupVarIndex var env

--           ClosureConversion.applyArgs args' $ pure term

--         Domain.Global global ->
--           ClosureConversion.convertGlobal global args'

--         Domain.Case scrutinee (Domain.Branches env' branches defaultBranch) ->
--           ClosureConversion.applyArgs args' $ do
--             scrutinee' <- readback env scrutinee
--             branches' <- case branches of
--               Syntax.ConstructorBranches constructorTypeName constructorBranches ->
--                 Syntax.ConstructorBranches constructorTypeName <$> OrderedHashMap.forMUnordered constructorBranches (readbackConstructorBranch env env')

--               Syntax.LiteralBranches literalBranches ->
--                 Syntax.LiteralBranches <$> OrderedHashMap.forMUnordered literalBranches (\branch -> do
--                   branchValue <- Evaluation.evaluate env' branch
--                   readback env branchValue
--                 )
--             defaultBranch' <- forM defaultBranch $ \branch -> do
--               branch' <- Evaluation.evaluate env' branch
--               readback env branch'
--             pure $ Syntax.Case scrutinee' branches' defaultBranch'

--     Domain.Con con params args ->
--       Syntax.Con con <$> mapM (readback env) params <*> mapM (readback env) args

--     Domain.Lit lit ->
--       pure $ Syntax.Lit lit

--     Domain.Pi name type_ closure ->
--       Syntax.Pi name <$> readback env type_ <*> readbackClosure env closure

--     Domain.Function tele ->
--       pure $ Syntax.Function tele

-- readbackConstructorBranch
--   :: Domain.Environment v
--   -> Domain.Environment v'
--   -> Telescope Syntax.Type Syntax.Term v'
--   -> M (Telescope Syntax.Type Syntax.Term v)
-- readbackConstructorBranch outerEnv innerEnv tele =
--   case tele of
--     Telescope.Empty term -> do
--       value <- Evaluation.evaluate innerEnv term
--       term' <- readback outerEnv value
--       pure $ Telescope.Empty term'

--     Telescope.Extend name domain plicity tele' -> do
--       domain' <- Evaluation.evaluate innerEnv domain
--       domain'' <- readback outerEnv domain'
--       (outerEnv', var) <- Environment.extend outerEnv
--       let
--         innerEnv' =
--           Environment.extendVar innerEnv var
--       tele'' <- readbackConstructorBranch outerEnv' innerEnv' tele'
--       pure $ Telescope.Extend name domain'' plicity tele''

-- readbackClosure :: Domain.Environment v -> Domain.Closure -> M (Scope Syntax.Term v)
-- readbackClosure env closure = do
--   (env', v) <- Environment.extend env
--   closure' <- Evaluation.evaluateClosure closure $ Domain.var v
--   readback env' closure'