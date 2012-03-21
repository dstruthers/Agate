module Main where
import Types
import Parser
import Compiler
import VM
import Control.Monad.Error
import Control.Monad.State
import System.IO

eval :: VM -> Op -> ThrowsError (SchemeValue, VM)
eval vm op = case runState (exec op) vm of
  (Right result, vm') -> return (result, vm')
  (Left error, vm') -> throwError error

repl :: VM -> IO ()
repl vm = do
  input <- prompt "agate> "
  case (parse input) >>= compile >>= (eval vm) of
    Right (result, vm') -> putStrLn (show result) >> repl vm'
    Left error -> putStrLn ("*** " ++ show error) >> repl vm

prompt p = putStr p >> hFlush stdout >> getLine

main = repl initialVM