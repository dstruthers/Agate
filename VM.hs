module VM where
import Control.Monad.Error (throwError)
import Control.Monad.State
import Data.Monoid

import Compiler
import Types

exec' :: Op -> VMState (ThrowsError SchemeValue)
exec' op = return $ Right (String (show op))

exec :: Op -> VMState (ThrowsError SchemeValue)
exec (Assign name next) = do
  vm <- get
  put vm { environment = envInsert name (accumulator vm) (environment vm) }
  exec next
  
exec (Constant k next) = do
  vm <- get
  put vm { accumulator = k }
  exec next

exec (Eval form next) = do
  vm <- get
  case accumulator vm of
    Environment e -> let vm' = vm { environment = (environment vm) `mappend` e
                                  , stack = Just (vm, next)
                                  }
                     in put vm' >> exec form
    badValue -> return . throwError $ TypeError "environment" badValue

exec (Frame returnOp next) = do
  vm <- get
  put vm { arguments = [], stack = Just (vm, returnOp) }
  exec next

exec Return = do
  vm <- get
  case stack vm of
    Just (vm', returnOp) -> do
      put vm' { accumulator = accumulator vm }
      exec returnOp
    Nothing -> return . throwError . GenericError $ "Return from empty stack"

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

exec (Combine args next) = do
  vm <- get
  case accumulator vm of
    Operative a e b -> execOperative a e b args next
    PrimitiveApplicative f -> do
      case application (toList args) (Apply f Return) of
        Right compiled -> exec (Frame next compiled)
        Left err -> return . throwError $ err
    badValue -> return . throwError . TypeError "operative" $ badValue

exec (PushArg next) = do
  vm <- get
  put vm { arguments = (arguments vm) ++ [accumulator vm] }
  exec next

exec (Apply f next) = do
  vm <- get
  execApplicative f next

exec badValue = return . throwError . RuntimeError $ badValue

application :: [SchemeValue] -> Op -> ThrowsError Op
application [] n = return n
application (a:as) n = do
  next <- application as n
  compile' a (PushArg next)

execOperative :: SchemeValue -> String -> Op -> SchemeValue -> Op -> VMState (ThrowsError SchemeValue)
execOperative params envVar body args next = do
  vm <- get
  let env = environment vm
      env' = newEnv $ envInsert envVar (Environment env) env
  put vm { accumulator = Environment env' }
  exec (Eval body next)
  where newEnv e = case params of
          Pair _ _ -> newEnvList e
          Symbol ps -> envInsert ps args e
        
        newEnvList e = foldr envBuilder e argPairs
        envBuilder (Symbol p, a) e = envInsert p a e
        argPairs = zip (toList params) (toList args)

execApplicative :: (VM -> ThrowsError SchemeValue) -> Op -> VMState (ThrowsError SchemeValue)
execApplicative f next = do
  vm <- get
  case (f vm) of
    Right result -> put vm { accumulator = result, arguments = [] } >> exec next
    Left err -> return . throwError $ err
