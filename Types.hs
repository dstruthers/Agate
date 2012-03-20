module Types where
import Control.Monad.Error

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
                 | GenericError String
                          
instance Show SchemeError where
  show (ParseError s) = "**ParseError: " ++ s
  show (TypeError s v) = "**TypeError: expected " ++ s ++ ", got " ++ show v
  show (GenericError s) = "**Error: " ++ s

instance Error SchemeError where
  noMsg  = GenericError "an error occurred"
  strMsg = GenericError

type ThrowsError = Either SchemeError

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