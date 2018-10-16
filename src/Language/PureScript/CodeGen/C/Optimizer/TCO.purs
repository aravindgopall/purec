module Language.PureScript.CodeGen.C.Optimizer.TCO
  ( tco
  ) where

import Prelude

import Data.Array ((:))
import Data.Array as A
import Data.Either (Either(..))
import Data.Foldable (all)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\))
import Debug.Trace (trace)
import Language.PureScript.CodeGen.C.AST (AST)
import Language.PureScript.CodeGen.C.AST as AST
import Language.PureScript.CodeGen.C.AST as Type
import Language.PureScript.CodeGen.Runtime as R

-- | Eliminate tail calls
-- |
-- | Transform a self-recursive function
tco :: AST -> AST
tco = AST.everywhere go
  where
  go x@(AST.VariableIntroduction v@{ name, initialization: Just (fn@AST.Function _) }) =
    let
      (outerArgs /\ innerArgs /\ replace /\ body) =
        let
          (argss /\ body /\ replace) = collectFnArgs fn
          innerArgs = fromMaybe [] $ A.head argss
          outerArgs = A.concat $ A.reverse $ fromMaybe [] $ A.tail argss
         in (outerArgs /\ innerArgs /\ replace /\ body)
    in
     if isTailRecursive name body
      then trace "here" \_->
        AST.VariableIntroduction $
          v { initialization =
                Just $ replace $ toLoop name outerArgs innerArgs body
            }
      else x
  go x = x

  tcoVar arg = "$_tco_var_" <> arg
  copyVar arg = "$_copy_" <> arg
  tcoDone = "$_tco_done"
  tcoLoop = "$_tco_loop"
  tcoResult = "$_tco_result"

  collectFnArgs = go [] identity
    where
    go acc f (AST.Function fn@{ arguments, body: Just (AST.Block sts) })
      | Just { head: body@(AST.Return _) } <- A.uncons sts =
      go (map _.name arguments : acc) <@> body $ \b ->
        f $
          AST.Function $ fn
            { arguments = arguments <#> \a -> a { name = copyVar a.name }
            , body = Just $ AST.Block [ b ]
            }

    go acc f (AST.Function fn@{ arguments, body: Just (body@(AST.Block _)) }) =
      (map _.name arguments : acc) /\ body /\ \b ->
        f $
          AST.Function $ fn
            { arguments = arguments <#> \a -> a { name = copyVar a.name }
            , body = Just b
            }

    go acc f (AST.Return (AST.Function fn@{ arguments, body: Just (AST.Block [ body ]) })) =
      go (map _.name arguments : acc) <@> body $ \b ->
        f $
          AST.Function $ fn
            { arguments = arguments <#> \a -> a { name = copyVar a.name }
            , body = Just $ AST.Block [ b ]
            }

    go acc f (AST.Return (AST.Function fn@{ arguments, body: Just body@(AST.Block _) })) =
      (map _.name arguments : acc) /\ body /\ \b ->
        f $
          AST.Return $
            AST.Function $ fn
              { arguments = arguments <#> \a -> a { name = copyVar a.name }
              , body = Just b
              }

    go acc f body =
      acc /\ body /\ f

  isTailRecursive :: String -> AST -> Boolean
  isTailRecursive ident ast =
   trace ({ ident, selfRefCount: countSelfReferences ast }) \_->
    countSelfReferences ast > 0 &&
      allInTailPosition ast

    where
    countSelfReferences = AST.everything (+) match
      where
      match (AST.Var ident') | ident == ident' = 1
      match _ = 0

    allInTailPosition = go
      where
      go (AST.Return expr)
        | isSelfCall ident expr =
            countSelfReferences expr == 1
        | otherwise =
            countSelfReferences expr == 0
      go (AST.While ast body) =
        countSelfReferences ast == 0 &&
          go body
      go (AST.IfElse ast body el) =
        countSelfReferences ast == 0 &&
          go body &&
            all go el
      go (AST.Block body) =
        A.all go body
      go (AST.VariableIntroduction { initialization }) =
        all ((_ == 0) <<< countSelfReferences) initialization
      go (AST.Assignment _ ast) =
        countSelfReferences ast == 0
      go _ = false

  toLoop :: String -> Array String -> Array String -> AST -> AST
  toLoop ident outerArgs innerArgs ast =
    AST.Block $
      let
        xs =
          outerArgs <#> \arg ->
            AST.VariableIntroduction
              { name: tcoVar arg
              , type: R.any
              , qualifiers: []
              , initialization: Just $ AST.Var $ copyVar arg
              }
        ys =
          [ AST.VariableIntroduction
              { name: tcoDone
              , type: Type.RawType "int" []
              , qualifiers: []
              , initialization: Just $ AST.NumericLiteral (Left 0)
              }
          , AST.VariableIntroduction
              { name: tcoResult
              , type: R.any
              , qualifiers: []
              , initialization: Nothing
              }
          , AST.Function
              { name: Just tcoLoop
              , arguments:
                  outerArgs <> innerArgs <#> \name ->
                    { name, type: R.any }
              , qualifiers: []
              , returnType: R.any
              , variadic:  false
              , body: Just $ AST.Block [ loopify ast ]
              }
          , AST.While (AST.Unary AST.Not (AST.Var tcoDone)) $
              AST.Block
                [ AST.Assignment (AST.Var tcoResult) $
                    AST.App (AST.Var tcoLoop) $
                      (map (AST.Var <<< tcoVar) outerArgs) <>
                      (map (AST.Var <<< copyVar) innerArgs)
                ]
          , AST.Return $ AST.Var tcoResult
          ]
      in xs <> ys

    where
    loopify :: AST -> AST
    loopify x = x

    loopify (AST.Return ret)
      | isSelfCall ident ret =
        let
          allArgumentValues = A.concat $ collectArgs [] ret
        in
          AST.Block $
            A.zipWith
              (\val arg -> AST.Assignment (AST.Var $ tcoVar arg) val)
              allArgumentValues outerArgs
            <>
            A.zipWith
              (\val arg -> AST.Assignment (AST.Var (copyVar arg)) val)
              (A.drop (A.length outerArgs) allArgumentValues)
              innerArgs
            <>
            [ AST.Return AST.Null ]
      | otherwise =
          AST.Block
            [ markDone
            , AST.Return ret
            ]
    loopify (AST.While cond body) = AST.While cond (loopify body)
    loopify (AST.IfElse cond body el) = AST.IfElse cond (loopify body) (loopify <$> el)
    loopify (AST.Block body) = AST.Block (map loopify body)
    loopify x = x

    markDone =
      AST.Assignment
        (AST.Var tcoDone)
        (AST.NumericLiteral (Left 1))

    collectArgs acc (AST.App fn args') = collectArgs (args' : acc) fn
    collectArgs acc _ = acc

  isSelfCall ident (AST.App (AST.Var ident') _) = ident == ident'
  isSelfCall ident (AST.App fn _) = isSelfCall ident fn
  isSelfCall _ _ = false
