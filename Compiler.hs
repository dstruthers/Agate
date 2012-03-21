module Compiler where
import Control.Monad.Error (throwError)
import Types

compile :: SchemeValue -> Op -> ThrowsError Op
compile (Symbol name) next = return $ Lookup name next
-- Literals
compile s@(String _) next = return $ Constant s next
compile n@(Number _) next = return $ Constant n next
compile b@(Boolean _) next = return $ Constant b next

-- Special Forms
compile (Pair (Symbol "define") (Pair (Symbol name) (Pair value Null))) next =
  compile value (Assign name next)
compile (Pair (Symbol "if") (Pair test (Pair conseq Null))) next =
  case compile conseq next of
    Right conseqComp -> compile test (Test conseqComp (Constant Null Exit))
    Left error -> throwError error
compile (Pair (Symbol "if") (Pair test (Pair conseq (Pair altern Null)))) next =
  case compile conseq next of
    Right conseqComp -> case compile altern next of
      Right alternComp -> compile test (Test conseqComp alternComp)
      Left error -> throwError error
    Left error -> throwError error   
compile badValue _ = throwError $ CompileError badValue