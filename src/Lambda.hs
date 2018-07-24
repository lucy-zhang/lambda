module Lambda where

import Text.Parsec
import Text.Parsec.String
import Test.QuickCheck
import Control.Monad
import System.IO
import Data.Set (Set)
import qualified Data.Set as Set

data VarName = VarName Char Int
    deriving (Eq, Show, Ord)

data Expr
    = Var VarName
    | Lambda VarName Expr
    | Appl Expr Expr
    deriving (Eq, Show)

varName :: Parser VarName
varName = do
    c <- letter
    i <- many digit
    return $ VarName c (if null i then 0 else read i :: Int)

var :: Parser Expr
var = fmap Var varName

lambda :: Parser Expr
lambda = do
    _ <- char '\\'
    n <- varName
    _ <- spaces
    _ <- char '.'
    e <- expr
    return (Lambda n e)

parensExpr :: Parser Expr
parensExpr = between (char '(') (char ')') expr

expr :: Parser Expr
expr = do
    exprs <- spaces *> many1 ((var <|> lambda <|> parensExpr) <* spaces)
    case exprs of
        e : [] -> return e
        e1 : e2 : es -> return $ foldl Appl (Appl e1 e2) es
        [] -> undefined

parseExpr :: String -> Either LambdaError Expr
parseExpr s = leftMap LambdaParseError $ parse expr "" s

substitute :: VarName -> Expr -> Expr -> Expr
substitute n e v@(Var n')
    | n == n'   = e
    | otherwise = v
substitute n e l@(Lambda n' e')
    | n == n'   = l
    | n' `freeIn` e  = Lambda n'' (substitute n e $ substitute n' (Var n'') e')
    | otherwise = Lambda n' (substitute n e e')
    where n'' = nextVarName e e' n'
substitute n e (Appl e1 e2) = Appl (substitute n e e1) (substitute n e e2)

nextVarName :: Expr -> Expr -> VarName -> VarName
nextVarName e e' (VarName c _) = head $ filter (`Set.notMember` used) candidates
    where used = Set.filter (\ (VarName c' _) -> c == c') $ Set.union (freeVars e) (freeVars e')
          candidates = map (VarName c) [0..]

reduce :: Expr -> (Expr, Bool)
reduce (Appl (Lambda n e) e') = (substitute n e' e, True)
reduce (Lambda n e) = (Lambda n e', r)
    where (e', r) = reduce e
reduce e@(Appl e1 e2) = case (r1, r2) of
    (True, _) -> (Appl e1' e2 , True)
    (_, True) -> (Appl e1  e2', True)
    (_, _)    -> (e, False)
    where (e1', r1) = reduce e1
          (e2', r2) = reduce e2
reduce e = (e, False)

data NonTerminatingEval = NoNormalForm | MaxRecursionDepthExceeded
    deriving (Eq, Show)

data LambdaError = LambdaParseError ParseError | LambdaEvalError NonTerminatingEval
    deriving (Eq, Show)

leftMap :: (a -> b) -> Either a c -> Either b c
leftMap f (Left a) = Left (f a)
leftMap _ (Right b) = Right b

eval :: Int -> Expr -> Either LambdaError Expr
eval depth ex = leftMap LambdaEvalError $ evalWithStack [] depth ex
    where
        evalWithStack :: [Expr] -> Int -> Expr -> Either NonTerminatingEval Expr
        evalWithStack _ 0 _ = Left MaxRecursionDepthExceeded
        evalWithStack stack i e = case reduce e of
            (e', False) -> Right e'
            (e', True) | e' `elem` stack -> Left NoNormalForm
            (e', True) -> evalWithStack (e' : stack) (i - 1) e'

freeIn :: VarName -> Expr -> Bool
freeIn n e = n `Set.member` freeVars e

freeVars :: Expr -> Set VarName
freeVars (Var n) = Set.singleton n
freeVars (Lambda n e) = Set.delete n (freeVars e)
freeVars (Appl e1 e2) = (freeVars e1) `Set.union` (freeVars e2)

parens :: String -> String
parens s = "(" ++ s ++ ")"

formatVarName :: VarName -> String
formatVarName (VarName c i) = [c] ++ (if i == 0 then [] else show i)

format :: Expr -> String
format (Var n) = formatVarName n
format (Lambda n e) = "\\" ++ formatVarName n ++ ". " ++ format e
format (Appl e1 e2) = f1 ++ " " ++ f2 where
    f1 = case e1 of
        Lambda _ _ -> parens $ format e1
        _          -> format e1
    f2 = case e2 of
        Var _      -> format e2
        _          -> parens $ format e2

recursionDepth :: Int
recursionDepth = 100

repl :: IO ()
repl = forever $ do
    hSetBuffering stdout NoBuffering
    putStr "> "
    s <- getLine
    case parseExpr s of
        Left pe -> putStrLn $ show pe
        Right e -> case eval recursionDepth e of
            Left nte -> putStrLn $ show nte
            Right rexp -> putStrLn $ format rexp

data BasisCombinator = S | K | I
    deriving (Eq, Show)
data CombExpr
    = CombVar VarName
    | Func BasisCombinator
    | CombAppl CombExpr CombExpr
    deriving (Eq, Show)

toCombExpr :: Expr -> CombExpr
toCombExpr (Var n) = CombVar n
toCombExpr (Appl e1 e2) = CombAppl (toCombExpr e1) (toCombExpr e2)
toCombExpr (Lambda n e)
    | n `freeIn` e = case e of
        Var _                      -> Func I
        Appl e1 (Var n') | n == n' -> toCombExpr e1
        Appl e1 e2                 -> CombAppl
                                        (CombAppl (Func S) (toCombExpr (Lambda n e1)))
                                        (toCombExpr (Lambda n e2))
        Lambda _ _                 -> convert (n, toCombExpr e)
    | otherwise = CombAppl (Func K) (toCombExpr e)
    where
        convert :: (VarName, CombExpr) -> CombExpr
        convert (n', e')
            | n' `freeInCombExpr` e' = case e' of
                CombVar _        -> Func I
                Func _             -> CombAppl (Func K) e'
                CombAppl e1' e2' -> CombAppl
                                    (CombAppl (Func S) (convert (n', e1')))
                                    (convert (n', e2'))
            | otherwise = CombAppl (Func K) e'

evalCombExpr :: CombExpr -> CombExpr
evalCombExpr (CombAppl (Func I) x) = evalCombExpr x
evalCombExpr (CombAppl (CombAppl (Func K) x) _) = evalCombExpr x
evalCombExpr (CombAppl (CombAppl (CombAppl (Func S) x) y) z) = evalCombExpr $
    CombAppl (CombAppl x z) (CombAppl y z)
evalCombExpr e@(CombAppl e1 e2)
    | e1 == e1' && e2 == e2' = e
    | otherwise = evalCombExpr $ CombAppl e1' e2'
    where
        e1' = evalCombExpr e1
        e2' = evalCombExpr e2
evalCombExpr e = e

freeInCombExpr :: VarName -> CombExpr -> Bool
freeInCombExpr n (CombVar n') = n == n'
freeInCombExpr _ (Func _) = False
freeInCombExpr n (CombAppl e1 e2) = freeInCombExpr n e1 || freeInCombExpr n e2

instance Arbitrary Expr where
    arbitrary = exprGen
    shrink = shrinkExpr

shrinkExpr :: Expr -> [Expr]
shrinkExpr (Var _) = []
shrinkExpr (Lambda n e) = [Var n, e] ++ map (Lambda n) (shrink e)
shrinkExpr (Appl e1 e2) = [e1, e2] ++ [Appl e1' e2' | (e1', e2') <- shrink (e1, e2)]

varNameGen :: Gen VarName
varNameGen = elements [ VarName 'x' 0, VarName 'y' 1, VarName 'z' 2 ]

exprGen :: Gen Expr
exprGen = sized sizedExprGen

sizedExprGen :: Int -> Gen Expr
sizedExprGen 0 = liftM Var varNameGen
sizedExprGen n = oneof [ liftM Var varNameGen
                       , liftM2 Lambda varNameGen subExpr
                       , liftM2 Appl subExpr subExpr ]
    where subExpr = sizedExprGen (n `div` 2)
