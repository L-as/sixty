{-# language OverloadedStrings #-}
module ReferenceCounting where

import Protolude hiding (Type, IntMap, IntSet, evaluate)

import qualified Binding
import qualified Applicative.Syntax as Syntax
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Environment
import Data.OrderedHashMap (OrderedHashMap)
import qualified Data.OrderedHashMap as OrderedHashMap
import qualified Index
import Literal (Literal)
import Monad
import Name (Name)
import qualified Name
import Syntax.Telescope (Telescope)
import qualified Syntax.Telescope as Telescope
import qualified Var
import Var (Var)

data InnerValue
  = OperandValue !InnerOperand
  | Con !Name.QualifiedConstructor [Operand] [Operand]
  | Let !Name !Var !Value !TypeOperand !Value
  | Function [(Name, Var, Type)] !Type
  | Apply !Name.Lifted [Operand]
  | Pi !Name !Var !Type !Type
  | Closure !Name.Lifted [Operand]
  | ApplyClosure !Value [Operand]
  | Case !Operand !Branches !(Maybe Value)
  deriving Show

data Value = Value !InnerValue (IntSet Var)
  deriving Show

type Type = Value

data InnerOperand
  = Var !Var
  | Global !Name.Lifted
  | Lit !Literal
  deriving Show

data Operand = Operand !InnerOperand (IntSet Var)
  deriving Show

type TypeOperand = Operand

data Branches
  = ConstructorBranches !Name.Qualified (OrderedHashMap Name.Constructor ([(Name, Var, Type)], Value))
  | LiteralBranches (OrderedHashMap Literal Value)
  deriving Show

type Occurrences = IntSet Var

type Environment = Environment.Environment Value

extend :: Environment v -> Type -> M (Environment (Index.Succ v), Var)
extend env type_ =
  Environment.extendValue env type_

extendVar :: Environment v -> Var -> Type -> Environment (Index.Succ v)
extendVar env var type_ =
  Environment.extendVarValue env var type_

lookupVarType :: Var -> Environment v -> Type
lookupVarType var env =
  fromMaybe (panic "ReferenceCounting.lookupVarType") $
  Environment.lookupVarValue var env

-------------------------------------------------------------------------------

occurrences :: Value -> Occurrences
occurrences (Value _ occs) =
  occs

operandOccurrences :: Operand -> Occurrences
operandOccurrences (Operand _ occs) =
  occs

makeVar :: Environment v -> Var -> Operand
makeVar env var =
  Operand (Var var) $
    IntSet.singleton var <>
    foldMap occurrences (Environment.lookupVarValue var env)

makeGlobal :: Name.Lifted -> Operand
makeGlobal global =
  Operand (Global global) mempty

makeLit :: Literal -> Operand
makeLit lit =
  Operand (Lit lit) mempty

makeOperand :: Operand -> Value
makeOperand (Operand operand occs) =
  Value (OperandValue operand) occs

makeCon :: Name.QualifiedConstructor -> [Operand] -> [Operand] -> Value
makeCon con params args =
  Value (Con con params args) $ foldMap operandOccurrences params <> foldMap operandOccurrences args

makeLet :: Name -> Var -> Value -> TypeOperand -> Value -> Value
makeLet name var value type_ body =
  Value (Let name var value type_ body) $
    occurrences value <>
    operandOccurrences type_ <>
    IntSet.delete var (occurrences body)

makeFunction :: [(Name, Var, Type)] -> Type -> Type
makeFunction args target =
  Value (Function args target) mempty -- Since it's closed

makeApply :: Name.Lifted -> [Operand] -> Value
makeApply name args =
  Value (Apply name args) $
    foldMap operandOccurrences args

makePi :: Name -> Var -> Type -> Value -> Value
makePi name var domain target =
  Value (Pi name var domain target) $
    occurrences domain <>
    IntSet.delete var (occurrences target)

makeClosure :: Name.Lifted -> [Operand] -> Value
makeClosure name args =
  Value (Closure name args) $
    foldMap operandOccurrences args

makeApplyClosure :: Value -> [Operand] -> Value
makeApplyClosure fun args =
  Value (ApplyClosure fun args) $
    foldMap operandOccurrences args

makeCase :: Operand -> Branches -> Maybe Value -> Value
makeCase scrutinee branches defaultBranch =
  Value (Case scrutinee branches defaultBranch) $
    operandOccurrences scrutinee <>
    branchOccurrences branches <>
    foldMap occurrences defaultBranch

branchOccurrences :: Branches -> Occurrences
branchOccurrences branches =
  case branches of
    ConstructorBranches _constructorTypeName constructorBranches ->
      foldMap (uncurry telescopeOccurrences) constructorBranches

    LiteralBranches literalBranches ->
      foldMap occurrences literalBranches

telescopeOccurrences :: [(Name, Var, Type)] -> Value -> Occurrences
telescopeOccurrences tele body =
  case tele of
    [] ->
      occurrences body

    (_, var, type_):tele' ->
      occurrences type_ <>
      IntSet.delete var (telescopeOccurrences tele' body)

-------------------------------------------------------------------------------

evaluate :: Environment v -> IntSet Var -> Syntax.Term v -> M Value
evaluate env ownedVars term =
  case term of
    Syntax.Operand (Syntax.Var index)
      | var `IntSet.member` ownedVars ->
        pure $ decreaseVars env (IntSet.delete var ownedVars) $ makeOperand $ makeVar env var

      | otherwise ->
        pure $
          decreaseVars env ownedVars $
          increaseVar env var $
          makeOperand $ makeVar env var
      where
        var = Environment.lookupIndexVar index env

    Syntax.Operand (Syntax.Global global) ->
      pure $ decreaseVars env ownedVars $ makeOperand $ makeGlobal global

    Syntax.Operand (Syntax.Lit lit) ->
      pure $ decreaseVars env ownedVars $ makeOperand $ makeLit lit

    Syntax.Con con params args ->
      makeCon con <$> mapM (evaluate env mempty) params <*> mapM (evaluate env ownedVars) args

    Syntax.Let name term' type_ body -> do
      type' <- evaluate env mempty type_
      term'' <- evaluate env ownedVars term'
      (env', var) <- extend env type'
      body' <- evaluate env' ownedVars body
      pure $ makeLet name var term'' type' body'

    Syntax.Function tele -> do
      result <- uncurry makeFunction <$> evaluateTelescope (Environment.emptyFrom env) mempty tele
      pure $ decreaseVars env ownedVars result

    Syntax.Apply global args ->
      makeApply global <$> mapM (evaluate env ownedVars) args

    Syntax.Pi name domain target -> do
      domain' <- evaluate env mempty domain
      (env', var) <- extend env domain'
      makePi name var domain' <$> evaluate env' mempty target

    Syntax.Closure global args ->
      makeClosure global <$> mapM (evaluate env ownedVars) args

    Syntax.ApplyClosure term' args ->
      makeApplyClosure <$> evaluate env ownedVars term' <*> mapM (evaluate env ownedVars) args

    Syntax.Case scrutinee branches defaultBranch ->
      makeCase <$>
        evaluate env ownedVars scrutinee <*>
        evaluateBranches env ownedVars branches <*>
        mapM (evaluate env ownedVars) defaultBranch

decreaseVars :: Environment v -> IntSet Var -> Value -> Value
decreaseVars = undefined

increaseVar :: Environment v -> Var -> Value -> Value
increaseVar = undefined

evaluateBranches
  :: Environment v
  -> IntSet Var
  -> Syntax.Branches v
  -> M Branches
evaluateBranches env ownedVars branches =
  case branches of
    Syntax.ConstructorBranches constructorTypeName constructorBranches ->
      ConstructorBranches constructorTypeName <$> OrderedHashMap.mapMUnordered (evaluateTelescope env ownedVars) constructorBranches

    Syntax.LiteralBranches literalBranches ->
      LiteralBranches <$> OrderedHashMap.mapMUnordered (evaluate env ownedVars) literalBranches

evaluateTelescope
  :: Environment v
  -> IntSet Var
  -> Telescope Syntax.Type Syntax.Term v
  -> M ([(Name, Var, Type)], Value)
evaluateTelescope env ownedVars tele =
  case tele of
    Telescope.Empty body -> do
      body' <- evaluate env ownedVars body
      pure ([], body')

    Telescope.Extend binding type_ _plicity tele' -> do
      type' <- evaluate env ownedVars type_
      (env', var) <- extend env type'
      (names, body) <- evaluateTelescope env' ownedVars tele'
      pure ((Binding.toName binding, var, type'):names, body)

-------------------------------------------------------------------------------

-- Insertion of reference count updates
--
-- * The caller of a function promises that the arguments are kept
-- alive during the call.
--
-- * Values are returned with an increased ref count.

-- insertOperations
--   :: Environment v
--   -> IntSet Var
--   -> Value
--   -> M Value
-- insertOperations env varsToDecrease value =
--   case value of
--     Var var
--       | IntSet.member var varsToDecrease ->
--         decreaseVars (IntSet.delete var varsToDecrease) value

--       | otherwise ->
--         increase value $
--         decreaseVars varsToDecrease value

--     Global _ ->
--       increase value $
--       decreaseVars varsToDecrease value

--     Con con params args ->
--       makeCon con <$> mapM (insertOperations mempty) args

--     Lit lit ->
--       decreaseVars varsToDecrease $ makeLit lit

--     Let name var value' type_ body ->
--       makeLet name var <$>
--         insertOperations env mempty value' <*>
--         insertOperations env mempty type_ <*>
--         insertOperations (extendVar env var type_) (IntSet.insert var varsToDecrease) body

--     Function domains target ->
--       pure $ makeFunction domains target

--     Apply global args ->
--       undefined

--     Pi name var domain target ->
--       pure $ makePi name var domain target

--     Closure global args ->
--       makeClosure global <$> mapM (insertOperations mempty) args

--     ApplyClosure fun args ->
--       undefined

--     Case scrutinee branches defaultBranch ->
--       undefined

-- decrease
--   :: Value
--   -> Value
--   -> M Value
-- decrease valueToDecrease k = do
--   var <- freshVar
--   pure $
--     makeLet
--       "dec"
--       var
--       (makeApply
--         (Name.Lifted "Sixten.Builtin.decreaseReferenceCount" 0)
--         [valueToDecrease]
--       )
--       (makeGlobal $ Name.Lifted "Sixten.Builtin.Unit" 0)
--       k

-- decreaseVars :: Environment v -> IntSet Var -> Value -> M Value
-- decreaseVars varsToDecrease value
--   | IntSet.null varsToDecrease =
--     pure value

--   | otherwise = do
--     var <- freshVar
--     pure $
--       makeLet "result" var value _
--       foldM decrease value $ makeVar <$> IntSet.toList varsToDecrease

-- increase
--   :: Value
--   -> Value
--   -> M Value
-- increase valueToDecrease k = do
--   var <- freshVar
--   pure $
--     makeLet
--       "inc"
--       var
--       (makeApply
--         (Name.Lifted "Sixten.Builtin.increaseReferenceCount" 0)
--         [valueToDecrease]
--       )
--       (makeGlobal $ Name.Lifted "Sixten.Builtin.Unit" 0)
--       k