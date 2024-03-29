module Compiler (compile, compile') where
import Control.Monad.Error (throwError)

import Types

compile :: LispValue -> ThrowsError Op
compile = (flip comp) Exit

compile' = comp

comp :: LispValue -> Op -> ThrowsError Op
comp (Symbol name) next = return $ Lookup name next
-- Literals
comp s@(String _) next = return $ Constant s next
comp n@(Number _) next = return $ Constant n next
comp b@(Boolean _) next = return $ Constant b next
comp e@(Environment _) next = return $ Constant e next

-- Special Forms
comp (Pair (Symbol "define") (Pair (Symbol name) (Pair value Null))) next =
  comp value (Assign name next)

comp (Pair (Symbol "vau") (Pair a (Pair (Symbol e) (Pair b Null)))) next = do
  body <- comp b Return
  if validParamList a
    then return $ Constant (Operative a e body) next
    else throwError $ GenericError "invalid argument list"

comp (Pair (Symbol "quote") (Pair form Null)) next =
  return $ Constant form next

comp (Pair combiner args) next = 
  comp combiner (Combine args next)

comp badValue _ = throwError $ CompileError badValue

validParamList :: LispValue -> Bool
validParamList (Symbol _) = True
validParamList x = isProperList x
