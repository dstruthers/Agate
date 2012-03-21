module Compiler where
import Control.Monad.Error (throwError)
import Types

compile :: SchemeValue -> ThrowsError Op
compile (Symbol name) = return $ Lookup name Exit
compile s@(String _) = return $ Constant s Exit
compile n@(Number _) = return $ Constant n Exit
compile b@(Boolean _) = return $ Constant b Exit
compile badValue = throwError $ CompileError badValue