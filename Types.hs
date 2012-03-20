module Types where

data Expr = Symbol  String
          | String  String
          | Number  Double
          | Boolean Bool
          | Pair    Expr Expr
          | Null          
            
instance Show Expr where
  show (Symbol s) = s
  show (String s) = show s
  show (Number n) = show n
  show (Boolean b) = if b then "#t" else "#f"
  show Null = "()"
  show (Pair x y) = "(" ++ showPair x y ++ ")"
    where showPair x (Pair y1 y2) = show x ++ " " ++ showPair y1 y2
          showPair x Null = show x
          showPair x y = show x ++ " . " ++ show y