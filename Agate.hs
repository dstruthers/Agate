module Agate where
import Types
import Parser
import Compiler
import VM

vm = initialVM

repl :: IO ()
repl = do
  putStr "agate> "
  input <- getLine
  case (parse input) >>= compile >>= (exec vm) of
    Right result -> putStrLn $ show result
    Left error -> putStrLn $ show error
  repl