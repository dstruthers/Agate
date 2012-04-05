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
                   ,("boolean?", PrimitiveApplicative (primOp $ return . Boolean . isBoolean))
                   ,("environment?", PrimitiveApplicative (primOp $ return . Boolean . isEnvironment))
                   ,("null?", PrimitiveApplicative (primOp $ return . Boolean . isNull))
                   ,("number?", PrimitiveApplicative (primOp $ return . Boolean . isNumber))
                   ,("pair?", PrimitiveApplicative (primOp $ return . Boolean . isPair))
                   ,("list", PrimitiveApplicative list)
                   ,("if", PrimitiveOperative ifOp)
                   ]
        compileEval vm code = parse code >>= compile >>= eval vm >>= return . snd
        defs = []

assert :: Bool -> LispError -> ThrowsError ()
assert True _ = return ()
assert False err = throwError err

assertArgComparison f n vm = do
  let args = arguments vm
      argCount = length args
  assert (argCount `f` n) (ArgCountError n argCount)

assertArgCount = assertArgComparison (==)
assertMinArgCount = assertArgComparison (>=)
assertMaxArgCount = assertArgComparison (<=)

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

list :: VM -> ThrowsError LispValue
list vm = let argCount = length (arguments vm)
          in if argCount > 0
             then return . fromList . arguments $ vm
             else throwError $ ArgCountError 1 argCount

ifOp :: VM -> ThrowsError LispValue
ifOp vm = do
  assertMinArgCount 2 vm
  assertMaxArgCount 3 vm
  
  compiled <- compile . head . arguments $ vm
  result <- eval vm compiled
  let args = arguments vm
  if isTrue (fst result)
    then compile (args !! 1) >>= eval vm >>= return . fst
    else if length args > 2
         then compile (args !! 2) >>= eval vm >>= return . fst
         else return Null