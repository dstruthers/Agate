module Primitives (initialVM) where
import Types
import Control.Monad.Error
import Data.Monoid

initialVM = VM Null env [] Nothing
  where env = foldr (\(k, v) e -> envInsert k v e) mempty bindings
        bindings = [("+", PrimitiveApplicative (numericBinOp (+) 0))
                   ,("-", PrimitiveApplicative (numericBinOp (-) 0))
                   ,("*", PrimitiveApplicative (numericBinOp (*) 1))
                   ,("/", PrimitiveApplicative (numericBinOp (/) 1))
                   ,("eval", PrimitiveApplicative eval)
                   ]

unpackNum :: SchemeValue -> ThrowsError Double
unpackNum (Number n) = return n
unpackNum badValue = throwError $ TypeError "number" badValue

numericBinOp :: (Double -> Double -> Double) -> Double -> VM -> ThrowsError SchemeValue
numericBinOp f id vm = do
  args <- mapM unpackNum (arguments vm)
  return . Number $ foldr f id args

assert :: Bool -> SchemeError -> ThrowsError ()
assert True _ = return ()
assert False err = throwError err

eval vm = do
  let args = arguments vm
      env = if length args > 1 then args !! 1 else Environment $ environment vm
  assert (isEnvironment env) (TypeError "environment" env)
  return (Symbol "foo")