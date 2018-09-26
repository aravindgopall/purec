module Language.PureScript.CodeGen.C.Optimizer.Common
  ( mapBlocks
  , replaceIdent
  , replaceIdents
  , isReassigned
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Language.PureScript.CodeGen.C.AST (AST, everything, everywhere)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.CompileError (CompileError(..))

mapBlocks :: (Array AST -> Array AST) -> AST -> AST
mapBlocks go (AST.Block sts) = AST.Block (go sts)
mapBlocks _  x = x

replaceIdent :: String -> AST -> AST -> AST
replaceIdent var1 ast = everywhere replace
  where
  replace (AST.Var var2) | var1 == var2 = ast
  replace x = x

replaceIdents :: Map String AST -> AST -> AST
replaceIdents vars = everywhere replace
  where
  replace v@(AST.Var var) = fromMaybe v $ Map.lookup var vars
  replace x = x

isReassigned :: String -> AST -> Boolean
isReassigned var1 = everything (||) go
  where
  go (AST.VariableIntroduction { name }) = var1 == name
  go (AST.Assignment (AST.Var name) _) = var1 == name
  go _ = false

isUpdated
  :: ∀ m
   . Monad m
  => MonadError CompileError m
  => String
  -> AST
  -> m Boolean
isUpdated var1 = everything (||) check
  where
  check (Assignment _ target _) | var1 == targetVariable target = True
  check _ = False

  targetVariable (AST.Var var) = pure var
  targetVariable (AST.Indexer _ tgt) = targetVariable tgt
  targetVariable (AST.Accessor _ tgt) = targetVariable tgt
  targetVariable _ =
    throwError $
      InternalError "Invalid argument to targetVariable"
