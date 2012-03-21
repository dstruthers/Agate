module VM where
import Control.Monad.Error (throwError)
import Control.Monad.State
import Types

exec :: Op -> VMState (ThrowsError SchemeValue)
exec (Constant k next) = do
  vm <- get
  put vm { accumulator = k }
  exec next

exec (Lookup v next) = do
  vm <- get
  case envLookup v (environment vm) of
    Just s -> put vm { accumulator = s } >> exec next
    Nothing -> return . throwError . ReferenceError $ v

exec (Test consequence alternative) = do
  vm <- get
  if isTrue (accumulator vm)
    then exec consequence
    else exec alternative

exec Exit = do
  vm <- get
  return . Right . accumulator $ vm

exec badValue = return . throwError . RuntimeError $ badValue
