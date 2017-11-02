module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp a elems 
  = fromJust (lookup a elems)

eval :: Exp -> Env -> Double
eval (Val a) _                 
  = a
eval (Id s) vals               
  = lookUp s vals
eval (UnApp op exp) vals 
  | op == Neg = (-) (0) (eval exp vals) 
  | otherwise = (findUnFunc op) (eval exp vals)
eval (BinApp op exp exp') vals 
  = (findBinFunc op) (eval exp vals) (eval exp' vals)

findUnFunc :: UnOp -> (Double -> Double)
findUnFunc op 
  = lookUp op unMapping
    where
      unMapping = [(Sin, sin),
                   (Cos, cos),
                   (Log, log)]
                      

findBinFunc :: BinOp -> (Double -> Double -> Double)
findBinFunc op
  = lookUp op binMapping
    where
      binMapping = [(Add, (+)),
                    (Mul, (*)),
                    (Div, (/))]


diff :: Exp -> String -> Exp
diff (Val a) s 
  = Val 0.0
diff (Id exp) s
  | exp == s  = Val 1.0
  | otherwise = Val 0.0
diff (BinApp Add e e') s
  = add (chain e s) (chain e' s)
diff (BinApp Mul e e') s
  | e == Val 0.0 || e' == Val 0.0 = Val 0.0
  | otherwise =  add (mult (e) (chain e' s)) (mult (chain e s) (e'))
diff (BinApp Div e e') s
  = divi (sub (mult (chain e s) (e')) (mult (e) (chain e' s))) (mult (e') (e'))
diff (UnApp Sin e) s
  | s == show(e) = UnApp Cos e 
  | otherwise = chain (UnApp Sin e) s
diff (UnApp Cos e) s
  | s == show(e) = UnApp Sin e
  | otherwise = chain (UnApp Cos e) s
diff exp@(UnApp Log e) s
  | s == show(e) = divi (Val 1.0) (e)
  | otherwise = chain exp s
diff (UnApp Neg e) s
  = UnApp Neg (diff e s)


chain :: Exp -> String -> Exp
chain exp@(UnApp (Neg) e) s  
  = diff exp s
chain (UnApp (Log) e) s      
  = divi (diff e s) (e)
chain exp@(UnApp (Cos) e) s  
  = UnApp Neg (mult (diff exp (show e)) (diff e s))
chain exp@(UnApp op e) s     
  = mult (diff exp (show e)) (diff e s)
chain exp@(BinApp op e e') s 
  = diff exp s
chain exp s                  
  = diff exp s

mult     = BinApp Mul
divi     = BinApp Div
add      = BinApp Add
sub e e' = BinApp Add e (UnApp Neg e')


maclaurin :: Exp -> Double -> Int -> Double
maclaurin exp val n
  = sumN n (map (flip eval [("x", val)]) (zipWith3 (createTerm) fs ds ps))
    where
      fs = scanl (*) (1) [1, 2 ..]
      ds = iterate (flip diff "x") exp
      ps  = ((Val 1) : iterate (mult (Id "x")) (Id "x")) 
      createTerm f d p
        = BinApp Div (BinApp Mul (Val (eval d [("x", 0.0)])) (p)) (Val f)

sumN :: Int -> [Double] -> Double
sumN n xs = sum (take n xs)

showExp :: Exp -> String
showExp (Val a) 
  = show(a) 
showExp (Id s) 
  = s
showExp (UnApp op exp) 
  = (findUnStr op) ++ (bracket (showExp exp))
showExp (BinApp op exp exp') 
  = bracket ((showExp exp) ++ (findBinStr op) ++ (showExp exp'))

bracket :: String -> String
bracket a = "(" ++ a ++ ")"

findUnStr :: UnOp -> String
findUnStr op 
  = lookUp op unMapping
    where
      unMapping = [(Sin, "sin"),
                   (Cos, "cos"),
                   (Log, "log"),
                   (Neg, "-")]
                      

findBinStr :: BinOp -> String
findBinStr op 
  = lookUp op binMapping
    where
      binMapping = [(Add, "+"),
                    (Mul, "*"),
                    (Div, "/")]


---------------------------------------------------------------------------
-- Test cases from the spec.

e1, e2, e3, e4, e5, e6 :: Exp

-- > 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- > x*x + y - 7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- > x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- > -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- > sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- > log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))

----------------------------------------------------------------------
-- EXTENSION: Uncomment and complete these...

instance Num Exp where
  negate      = UnApp Neg
  (+)         = BinApp Add
  (*)         = BinApp Mul
  fromInteger x = Val (fromInteger x) 

instance Fractional Exp where
  (/)    = BinApp Div

instance Floating Exp where
  sin    = UnApp Sin
  cos    = UnApp Cos
  log    = UnApp Log

-- instance (Eq a, Num a) => Num (Maybe a) where

-- instance (Eq a, Fractional a) => Fractional (Maybe a) where

-- diff2 :: Exp -> String -> Maybe Exp



-- The following makes it much easier to input expressions, e.g. sin x, log(x*x) etc.

x, y :: Exp
x = Id "x"
y = Id "y"
