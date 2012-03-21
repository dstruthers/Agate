module Agate where
import Types
import Parser
import Compiler
import VM
import Control.Monad.Error
import Control.Monad.State

vm = initialVM

eval :: VM -> Op -> ThrowsError (SchemeValue, VM)
eval vm op = case runState (exec op) vm of
  (Right result, vm') -> return (result, vm')
  (Left error, vm') -> throwError error

repl :: VM -> IO ()
repl vm = do
  putStr "agate> "
  input <- getLine
  case (parse input) >>= (flip compile) Exit >>= (eval vm) of
    Right (result, vm') -> putStrLn (show result) >> repl vm'
    Left error -> putStrLn ("*** " ++ show error) >> repl vm

main = repl initialVM