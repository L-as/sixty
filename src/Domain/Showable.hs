{-# language DuplicateRecordFields #-}
{-# language GADTs #-}
{-# language StandaloneDeriving #-}
module Domain.Showable where

import Protolude hiding (Type, IntMap, force, to)

import Data.Tsil (Tsil)
import qualified Domain
import qualified Environment
import Index
import Literal (Literal)
import Monad
import qualified Name
import Name (Name)
import Plicity
import qualified Syntax

data Value
  = Neutral !Domain.Head Spine
  | Con !Name.QualifiedConstructor (Tsil (Plicity, Value))
  | Lit !Literal
  | Glued !Domain.Head Spine !Value
  | Lam !Name !Type !Plicity !Closure
  | Pi !Name !Type !Plicity !Closure
  | Fun !Type !Plicity !Type
  deriving Show

type Type = Value

newtype Spine = Spine (Tsil Elimination)
  deriving Show

data Elimination
  = Apps (Tsil (Plicity, Value))
  | Case !Branches
  deriving Show

type Environment = Environment.Environment Value

data Closure where
  Closure :: Environment v -> Scope Syntax.Term v -> Closure

deriving instance Show Closure

data Branches where
  Branches :: Environment v -> Syntax.Branches v -> Maybe (Syntax.Term v) -> Branches

deriving instance Show Branches

to :: Domain.Value -> M Value
to value =
  case value of
    Domain.Neutral hd (Domain.Spine spine) ->
      Neutral hd . Spine <$> mapM eliminationTo spine

    Domain.Con con args ->
      Con con <$> mapM (mapM to) args

    Domain.Lit lit ->
      pure $ Lit lit

    Domain.Glued hd (Domain.Spine spine) value' ->
      Glued hd . Spine <$> mapM eliminationTo spine <*> lazyTo value'

    Domain.Lam name type_ plicity closure ->
      Lam name <$> to type_ <*> pure plicity <*> closureTo closure

    Domain.Pi name type_ plicity closure ->
      Pi name <$> to type_ <*> pure plicity <*> closureTo closure

    Domain.Fun domain plicity target ->
      Fun <$> to domain <*> pure plicity <*> to target

eliminationTo :: Domain.Elimination -> M Elimination
eliminationTo elimination =
  case elimination of
    Domain.Apps args ->
      Apps <$> mapM (mapM to) args

    Domain.Case branches ->
      Case <$> branchesTo branches

lazyTo :: Lazy Domain.Value -> M Value
lazyTo =
  to <=< force

closureTo :: Domain.Closure -> M Closure
closureTo (Domain.Closure env term) =
  flip Closure term <$> environmentTo env

branchesTo :: Domain.Branches -> M Branches
branchesTo (Domain.Branches env branches defaultBranch) = do
  env' <- environmentTo env
  pure $ Branches env' branches defaultBranch

environmentTo :: Domain.Environment v -> M (Environment v)
environmentTo env = do
  values' <- mapM to $ Environment.values env
  pure Environment.Environment
    { scopeKey = Environment.scopeKey env
    , indices = Environment.indices env
    , values = values'
    }
