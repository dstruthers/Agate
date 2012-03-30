module Types where
import Control.Monad.Error
import Control.Monad.State
import Data.Map as Map hiding (toList)
import Data.Monoid

data LispValue = Symbol  String
               | String  String
               | Number  Double
               | Boolean Bool
               | Pair    LispValue LispValue
               | Null
               | Environment Env
               | Operative LispValue String Op
               | PrimitiveApplicative (VM -> ThrowsError LispValue)
            
instance Show LispValue where
  show (Symbol s) = s
  show (String s) = show s
  show (Number n) = show n
  show (Boolean b) = if b then "#t" else "#f"
  show Null = "()"
  show (Pair x y) = "(" ++ showPair x y ++ ")"
    where showPair x (Pair y1 y2) = show x ++ " " ++ showPair y1 y2
          showPair x Null = show x
          showPair x y = show x ++ " . " ++ show y
  show (Environment _) = "#<environment>"
  show (Operative _ _ _) = "#<compound operative>"
  show (PrimitiveApplicative _) = "#<primitive applicative>"

data LispError = ParseError String
               | TypeError String LispValue
               | CompileError LispValue
               | ReferenceError String
               | RuntimeError Op
               | ArgCountError Int Int
               | GenericError String
                          
instance Show LispError where
  show (ParseError s) = "ParseError: " ++ s
  show (TypeError s v) = "TypeError: expected " ++ s ++ ", got " ++ show v
  show (CompileError v) = "CompilationError: cannot compile " ++ show v
  show (ReferenceError v) = "ReferenceError: unbound symbol " ++ v
  show (ArgCountError e r) = "ArgCountError: expected " ++ show e ++ " argument" ++ if e == 1 then "" else "s" ++ ", got " ++ show r
  show (RuntimeError o) = "RuntimeError: cannot execute " ++ show o
  show (GenericError s) = "Error: " ++ s

instance Error LispError where
  noMsg  = GenericError "an error occurred"
  strMsg = GenericError

type ThrowsError = Either LispError

data Op = Assign String Op
        | Constant LispValue Op
        | Lookup String Op
        | Test Op Op
        | Exit
        | Eval Op Op
        | PushArg Op
        | Frame Op Op
        | Apply (VM -> ThrowsError LispValue) Op
        | Return
        | Combine LispValue Op
          
instance Show Op where
  show (Assign s o) = "Assign " ++ show s ++ " >>> " ++ show o
  show (Constant s o) = "Constant " ++ show s ++ " >>> " ++ show o
  show (Lookup s o) = "Lookup " ++ show s ++ " >>> " ++ show o
  show (Test c a) = "Test { Consequence = " ++ show c ++ "} { Alternative = " ++ show a ++ " }"
  show Exit = "Exit"
  show (Eval o n) = "Eval (" ++ show o ++ ") >>> " ++ show n
  show (PushArg o) = "PushArg >>> " ++ show o
  show (Frame r n) = "Frame (ret = " ++ show r ++ ") >>> " ++ show n
  show (Apply _ o) = "Apply >>> " ++ show o
  show Return = "Return"
  show (Combine s o) = "Combine " ++ show s ++ " >>> " ++ show o

data VM = VM
          { accumulator :: LispValue
          , environment :: Env
          , arguments   :: [LispValue]
          , stack       :: Maybe (VM, Op)
          }
        deriving (Show)

newtype Env = Env
              {
                unEnv :: Map.Map String LispValue
              }
            deriving (Show)

instance Monoid Env where
  mempty = Env Map.empty
  mappend (Env a) (Env b) = Env $ Map.union a b

type VMState a = State VM a

envLookup :: String -> Env -> Maybe LispValue
envLookup s = (Map.lookup s) . unEnv

envInsert :: String -> LispValue -> Env -> Env
envInsert s v = Env . (Map.insert s v) . unEnv

typeof :: LispValue -> String
typeof (Symbol _) = "symbol"
typeof (String _) = "string"
typeof (Number _) = "number"
typeof (Boolean _) = "boolean"
typeof (Pair _ _) = "pair"
typeof Null = "null"

isSymbol (Symbol _) = True
isSymbol _ = False

isString (String _) = True
isString _ = False

isNumber (Number _) = True
isNumber _ = False

isBoolean (Boolean _) = True
isBoolean _ = False

isPair (Pair _ _) = True
isPair _ = False

isEnvironment (Environment _) = True
isEnvironment _ = False

car :: LispValue -> ThrowsError LispValue
car (Pair x _) = return x
car badValue   = throwError $ TypeError "pair" badValue

cdr :: LispValue -> ThrowsError LispValue
cdr (Pair _ y) = return y
cdr badValue   = throwError $ TypeError "pair" badValue

cons :: LispValue -> LispValue -> ThrowsError LispValue
cons x y = return $ Pair x y

isNull Null = True
isNull _ = False

fromList :: [LispValue] -> LispValue
fromList = foldr Pair Null

toList :: LispValue -> [LispValue]
toList Null = []
toList (Pair x xs) = x : toList xs

isTrue (Boolean False) = False
isTrue _               = True

isFalse = not . isTrue

isProperList Null = True
isProperList (Pair _ cdr) = isProperList cdr
isProperList _ = False
