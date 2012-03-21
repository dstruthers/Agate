module Types where
import Control.Monad.Error
import Control.Monad.State
import Data.Map as Map

data SchemeValue = Symbol  String
                | String  String
                | Number  Double
                | Boolean Bool
                | Pair    SchemeValue SchemeValue
                | Null          
            
instance Show SchemeValue where
  show (Symbol s) = s
  show (String s) = show s
  show (Number n) = show n
  show (Boolean b) = if b then "#t" else "#f"
  show Null = "()"
  show (Pair x y) = "(" ++ showPair x y ++ ")"
    where showPair x (Pair y1 y2) = show x ++ " " ++ showPair y1 y2
          showPair x Null = show x
          showPair x y = show x ++ " . " ++ show y

data SchemeError = ParseError String
                 | TypeError String SchemeValue
                 | CompileError SchemeValue
                 | ReferenceError String
                 | RuntimeError Op
                 | GenericError String
                          
instance Show SchemeError where
  show (ParseError s) = "ParseError: " ++ s
  show (TypeError s v) = "TypeError: expected " ++ s ++ ", got " ++ show v
  show (CompileError v) = "CompilationError: cannot compile " ++ show v
  show (ReferenceError v) = "ReferenceError: unbound symbol " ++ v
  show (RuntimeError o) = "RuntimeError: cannot execute " ++ show o
  show (GenericError s) = "Error: " ++ s

instance Error SchemeError where
  noMsg  = GenericError "an error occurred"
  strMsg = GenericError

type ThrowsError = Either SchemeError

data Op = Constant SchemeValue Op
        | Lookup String Op
        | Test Op Op
        | Exit
        deriving (Show)

data VM = VM
          { accumulator :: SchemeValue
          , environment :: Env
          }
        deriving (Show)

type Env = Map.Map String SchemeValue
type VMState a = State VM a

initialVM = VM Null Map.empty

envLookup :: String -> Env -> Maybe SchemeValue
envLookup = Map.lookup

envInsert :: String -> SchemeValue -> Env -> Env
envInsert = Map.insert

typeof :: SchemeValue -> String
typeof (Symbol _) = "symbol"
typeof (String _) = "string"
typeof (Number _) = "number"
typeof (Boolean _) = "boolean"
typeof (Pair _ _) = "pair"
typeof Null = "null"

car :: SchemeValue -> ThrowsError SchemeValue
car (Pair x _) = return x
car badValue   = throwError $ TypeError "pair" badValue

cdr :: SchemeValue -> ThrowsError SchemeValue
cdr (Pair _ y) = return y
cdr badValue   = throwError $ TypeError "pair" badValue

cons = Pair

fromList :: [SchemeValue] -> SchemeValue
fromList = foldr Pair Null

isTrue (Boolean False) = False
isTrue _               = True