module Compiler (compile) where
import Control.Monad.Error (throwError)
import Types

compile = (flip comp) Exit

comp :: SchemeValue -> Op -> ThrowsError Op
comp (Symbol name) next = return $ Lookup name next
-- Literals
comp s@(String _) next = return $ Constant s next
comp n@(Number _) next = return $ Constant n next
comp b@(Boolean _) next = return $ Constant b next

-- Special Forms
comp (Pair (Symbol "define") (Pair (Symbol name) (Pair value Null))) next =
  comp value (Assign name next)
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

comp badValue _ = throwError $ CompileError badValue