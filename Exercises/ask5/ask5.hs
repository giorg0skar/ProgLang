import Data.Char
import System.IO
import Text.Read
import Data.Map (Map)

data Type  =  Tvar Int | Tfun Type Type                        deriving Eq
data Expr  =  Evar String | Eabs String Expr | Eapp Expr Expr  deriving Eq


check :: Maybe Type -> Bool
check Nothing  = False
check (Just a) = True

getM :: Maybe Type -> Type
getM Nothing = Tvar 101
getM (Just a)  = a

--walg c:cs | = 

--function to infer type of an expression. n is the current number to be assigned when we encounter a variable
-- infer :: Int -> Map String Type -> [Expr] -> Maybe Type
-- infer n env [] = 
-- infer n env (Evar x):es     = if (check (lookup x env)) then lookup x env else Just (Tvar n)
-- infer n env (Eabs x e):es   = Tfun (Tvar n) (infer n+1 (insert x (Tvar n) env) e:es)
-- infer n env (Eapp e1 e2):es = infer n+1 env e1:e2:es


-- infer :: Int -> Map String Type -> Expr -> Type
-- infer n env (Evar x)     = if (check (lookup x env)) then getM (lookup x env) else (Tvar n)
-- infer n env (Eabs x e)   = Tfun (Tvar n) (infer n+1 (insert x (Tvar n) env) e)
-- infer n env (Eapp e1 e2) = Tfun (infer n env e2) (infer n env e1)

infer :: Int -> Map String Type -> Map (String, Type) (String, Type) -> [Expr] -> (Map String Type, Map Type Type)
infer n env c [] = (env,c)
infer n env c (Evar x):es = if (check (lookup x env)) then (infer n env c es) else (infer n+1 (insert x (Tvar n) env) c es)
infer n env c (Eabs x e):es = infer n+1 (insert x (Tvar n) env) c e:es
-- infer n env c (Eabs x e):es = infer n+2 newenv c e:es
--   where newenv = insert ("\\." ++ x ++ " " ++ show e) (Tvar n+1) . (insert x (Tvar n) env)
infer n env c (Eapp e1 e2):es = infer n+1 (insert (show e1 ++ " " ++ show e2) (Tvar n) env) (insert t1 t2 c) (e1:e2:es)
  where Just t1 = lookup (show e1) env
        Just t2 = lookup (show e2) env


-- Pretty printing of expressions

always = True    -- False omits parentheses whenever possible

instance Show Expr where
  showsPrec p (Evar x) = (x ++)
  showsPrec p (Eabs x e) =
    showParen (always || p > 0) ((("\\" ++ x ++ ". ") ++) . showsPrec 0 e)
  showsPrec p (Eapp e1 e2) =
    showParen (always || p > 1) (showsPrec 1 e1 . (" " ++) . showsPrec 2 e2)

-- Parsing of expressions

instance Read Expr where
  readPrec = (do Ident x <- lexP
                 return (Evar x)) <++
             (do Punc "(" <- lexP
                 Punc "\\" <- lexP
                 Ident x <- lexP
                 Symbol "." <- lexP
                 e <- readPrec
                 Punc ")" <- lexP
                 return (Eabs x e)) <++
             (do Punc "(" <- lexP
                 e1 <- readPrec
                 e2 <- readPrec
                 Punc ")" <- lexP
                 return (Eapp e1 e2))

-- Pretty printing of types

instance Show Type where
  showsPrec p (Tvar alpha) = ("@" ++) . showsPrec 0 alpha
  showsPrec p (Tfun sigma tau) =
    showParen (p > 0) (showsPrec 1 sigma . (" -> " ++) . showsPrec 0 tau)

-- Main program

readOne  =  do  s <- getLine
                let e = read s :: Expr
                putStrLn ("Parsed: " ++ show e)
                --let ty = infer 0 empty  empty [e]
                --print ty
                --if ty==Nothing then print "type error" else print (get ty)

count n m  =  sequence $ take n $ repeat m

main     =  do  n <- readLn
                count n readOne