module Compiler (compile, compile') where
import Control.Monad.Error (throwError)

import Types

compile :: SchemeValue -> ThrowsError Op
compile = (flip comp) Exit

compile' = comp

comp :: SchemeValue -> Op -> ThrowsError Op
comp (Symbol name) next = return $ Lookup name next
-- Literals
comp s@(String _) next = return $ Constant s next
comp n@(Number _) next = return $ Constant n next
comp b@(Boolean _) next = return $ Constant b next
comp e@(Environment _) next = return $ Constant e next

-- Special Forms
comp (Pair (Symbol "define") (Pair (Symbol name) (Pair value Null))) next =
  comp value (Assign name next)
{-comp (Pair (Symbol "eval") (Pair form (Pair e Null))) next =
  comp form Return >>= \c -> comp e (Eval c next)
comp (Pair (Symbol "eval") (Pair form Null)) next =
  comp form next
-}
comp (Pair (Symbol "vau") (Pair a (Pair (Symbol e) (Pair b Null)))) next = do
  body <- comp b Return
  if validParamList a
    then return $ Constant (Operative a e body) next
    else throwError $ GenericError "invalid argument list"

-- TODO: refactor into monadic version
comp (Pair (Symbol "if") (Pair test (Pair conseq Null))) next =
  case comp conseq next of
    Right conseqComp -> comp test (Test conseqComp (Constant Null Exit))
    Left error -> throwError error
comp (Pair (Symbol "if") (Pair test (Pair conseq (Pair altern Null)))) next =
  case comp conseq next of
    Right conseqComp -> case comp altern next of
      Right alternComp -> comp test (Test conseqComp alternComp)
      Left error -> throwError error
    Left error -> throwError error   

comp (Pair (Symbol "quote") (Pair form Null)) next =
  return $ Constant form next

comp (Pair combiner args) next = 
  comp combiner (Combine args next)

comp badValue _ = throwError $ CompileError badValue

validParamList :: SchemeValue -> Bool
validParamList (Symbol _) = True
validParamList x = isProperList x
