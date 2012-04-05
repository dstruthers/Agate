module VM where
import Control.Monad.Error (throwError)
import Control.Monad.State
import Data.Monoid

import Compiler
import Types

eval :: VM -> Op -> ThrowsError (LispValue, VM)
eval vm op = case runState (exec op) vm of
  (Right result, vm') -> return (result, vm')
  (Left error, vm') -> throwError error

exec' :: Op -> VMState (ThrowsError LispValue)
exec' op = return $ Right (String (show op))

exec :: Op -> VMState (ThrowsError LispValue)
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
    PrimitiveOperative f -> execPrimitiveOperative f args next
    PrimitiveApplicative f -> execPrimitiveApplicative f args next
    badValue -> return . throwError . TypeError "operative or applicative" $ badValue

exec (PushArg next) = do
  vm <- get
  put vm { arguments = (arguments vm) ++ [accumulator vm] }
  exec next

exec (Invoke f next) = do
  vm <- get
  execCombiner f next

exec badValue = return . throwError . RuntimeError $ badValue

application :: [LispValue] -> Op -> ThrowsError Op
application [] n = return n
application (a:as) n = do
  next <- application as n
  compile' a (PushArg next)
  
operation :: [LispValue] -> Op -> ThrowsError Op
operation [] n = return n
operation (a:as) n = do
  next <- operation as n
  return $ Constant a (PushArg next)

execOperative :: LispValue -> String -> Op -> LispValue -> Op -> VMState (ThrowsError LispValue)
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

execCombiner :: (VM -> ThrowsError LispValue) -> Op -> VMState (ThrowsError LispValue)
execCombiner f next = do
  vm <- get
  case (f vm) of
    Right result -> put vm { accumulator = result, arguments = [] } >> exec next
    Left err -> return . throwError $ err

execPrimitiveApplicative :: (VM -> ThrowsError LispValue) -> LispValue -> Op -> VMState (ThrowsError LispValue)
execPrimitiveApplicative = optimizeCombiner application

execPrimitiveOperative :: (VM -> ThrowsError LispValue) -> LispValue -> Op -> VMState (ThrowsError LispValue)
execPrimitiveOperative = optimizeCombiner operation

optimizeCombiner :: ([LispValue] -> Op -> ThrowsError Op) 
                    -> (VM -> ThrowsError LispValue)
                    -> LispValue
                    -> Op
                    -> VMState (ThrowsError LispValue)
optimizeCombiner mode f args next = do
  case next of
    Return -> case mode (toList args) (Invoke f next) of
      Right compiled -> exec compiled
      Left err -> return . throwError $ err
      
    _ -> case mode (toList args) (Invoke f Return) of
      Right compiled -> exec (Frame next compiled)
      Left err -> return . throwError $ err
