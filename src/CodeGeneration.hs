{-# language GeneralizedNewtypeDeriving #-}
module CodeGeneration where

import Protolude hiding (State, StateT)

import qualified Applicative.Syntax as Syntax
import qualified Environment
import qualified Control.Monad.State.Strict as Strict
import qualified Control.Monad.State.Lazy as Lazy

import LLVM.IRBuilder (IRBuilder)
import qualified LLVM.IRBuilder as IRBuilder
import qualified LLVM.AST as LLVM

newtype Builder a = Builder { unBuilder :: Strict.State BuilderState a }
  deriving (Functor, Applicative, Monad)

data BuilderState = BuilderState
  { _irBuilder :: !IRBuilder.IRBuilderState
  , _moduleBuilder :: !IRBuilder.ModuleBuilderState
  }

instance IRBuilder.MonadIRBuilder Builder where
  liftIRState (Strict.StateT s) =
    Builder $ Strict.StateT $ \builderState ->
      second (\fb -> builderState { _irBuilder = fb }) <$>
        s (_irBuilder builderState)

instance IRBuilder.MonadModuleBuilder Builder where
  liftModuleState (Lazy.StateT s) =
    Builder $ Strict.StateT $ \builderState ->
      second (\mb -> builderState { _moduleBuilder = mb }) <$>
        s (_moduleBuilder builderState)

type Environment = Environment.Environment LLVM.Operand

data Return = OutParameter
  { _type :: LLVM.Operand
  , _location :: LLVM.Operand
  }

generate :: Environment v -> Syntax.Term v -> Return -> Builder LLVM.Operand
generate env term return_ =
  case term of
    Syntax.Operand operand ->
      generateOperand env operand return_

    Syntax.Con con params args ->
      undefined

    Syntax.Let name term type' body ->
      undefined

    Syntax.Function tele ->
      undefined

    Syntax.Apply global args ->
      undefined

    Syntax.Pi name domain target ->
      undefined

    Syntax.Closure global args ->
      undefined

    Syntax.ApplyClosure operand args ->
      undefined

    Syntax.Case scrutinee branches defaultBranch ->
      undefined

generateOperand :: Environment v -> Syntax.Operand v -> Return -> Builder LLVM.Operand
generateOperand env operand =
  case operand of
    Syntax.Var var ->
      undefined

    Syntax.Global global ->
      undefined

    Syntax.Lit lit ->
      undefined
