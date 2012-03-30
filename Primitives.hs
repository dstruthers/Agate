module Primitives (initialVM) where
import Control.Monad.Error
import Data.Monoid

import Compiler
import Parser
import Types
import VM

initialVM :: ThrowsError VM
initialVM = foldM compileEval vm defs
  where vm = VM Null env [] Nothing
        env = foldr (\(k, v) e -> envInsert k v e) mempty bindings
        bindings = [("+", PrimitiveApplicative (numericBinOp (+) 0))
                   ,("-", PrimitiveApplicative (numericBinOp (-) 0))
                   ,("*", PrimitiveApplicative (numericBinOp (*) 1))
                   ,("/", PrimitiveApplicative (numericBinOp (/) 1))
                   ,("car", PrimitiveApplicative (primOp car))
                   ,("cdr", PrimitiveApplicative (primOp cdr))
                   ,("cons", PrimitiveApplicative (primOp2 cons))
                   ,("boolean?", PrimitiveApplicative (primOp $ return . Boolean .isBoolean))
                   ,("environment?", PrimitiveApplicative (primOp $ return . Boolean . isEnvironment))
                   ,("null?", PrimitiveApplicative (primOp $ return . Boolean . isNull))
                   ,("number?", PrimitiveApplicative (primOp $ return . Boolean . isNumber))
                   ,("pair?", PrimitiveApplicative (primOp $ return . Boolean . isPair))
                   ]
        compileEval vm code = parse code >>= compile >>= eval vm >>= return . snd
        defs = []

assert :: Bool -> LispError -> ThrowsError ()
assert True _ = return ()
assert False err = throwError err

assertArgCount :: Int -> VM -> ThrowsError ()
assertArgCount n vm = do
  let args = arguments vm
      argCount = length args
  assert (argCount == n) (ArgCountError n argCount)

primOp :: (LispValue -> ThrowsError LispValue) -> VM -> ThrowsError LispValue
primOp f vm = do
  assertArgCount 1 vm
  let args = arguments vm
  f (head args)

primOp2 :: (LispValue -> LispValue -> ThrowsError LispValue) -> VM -> ThrowsError LispValue
primOp2 f vm = do
  assertArgCount 2 vm
  let args = arguments vm
  f (args !! 0) (args !! 1)

unpackNum :: LispValue -> ThrowsError Double
unpackNum (Number n) = return n
unpackNum badValue = throwError $ TypeError "number" badValue

numericBinOp :: (Double -> Double -> Double) -> Double -> VM -> ThrowsError LispValue
numericBinOp f id vm = do
  args <- mapM unpackNum (arguments vm)
  return . Number $ foldr f id args
