module TermLanguage where

--- | Original code taken from: http://www.willamette.edu/~fruehr/haskell/evolution.html
--- | Modified by Kyle Jones for COP-4020

-- a dynamically-typed term language

import Data.Maybe
import Data.Char (isSpace)
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO

data Term = Use Var
          | Lit Integer
          | App Term Term
          | Abs Var  Term
          | Rec Var  Term

type Var  = String

-- a domain of values, including functions

data Value = Num Integer
           | Bool Bool
           | Fun (Value -> Value)
           | Let String Value --- | Tacked on for repl

instance Show Value where
  show (Num  n) = show n
  show (Bool b) = show b
  show (Let n _) = n ++ " :: Value -> Value"
  show  _  = ""

prjFun :: Value -> Value -> Value
prjFun (Fun f) = f
prjFun  _      = error "bad function value"

prjNum :: Value -> Integer
prjNum (Num n)  = n
prjNum (Bool b) = if b then 1 else 0
prjNum  _       = error "bad integer value"

prjBool :: Value -> Bool
prjBool (Bool b) = b
prjBool (Num n)  = n /= 0
prjBool  _       = error "bad boolean value"

binOp inj f = Fun (\i -> (Fun (\j -> inj (f (prjNum i) (prjNum j)))))

-- environments mapping variables to values

type Env = [(Var, Value)]

getval :: Var -> Env -> Value
getval x env = fromMaybe (error ("no value for " ++ x)) (lookup x env) 

hasval :: Var -> Env -> Bool
hasval x env = case lookup x env of
                Just _     -> True
                Nothing    -> False

define :: Var -> Value -> Env -> Env
define x v []            = [(x,v)]
define x v (e@(n,v1):es) = if x == n then (x,v) : es else e : define x v es

-- an environment-based evaluation function

eval :: Env -> Term -> Value
eval env (Use c)   = if hasval c env then getval c env else getval c prims
eval env (Lit k)  = Num k
eval env (App m n) = prjFun (eval env m) (eval env n)
eval env (Abs x m) = Fun  (\v -> eval ((x,v) : env) m)
eval env (Rec x m) = f where f = eval ((x,f) : env) m


-- a (fixed) "environment" of language primitives

times = binOp Num  (*)
minus = binOp Num  (-)
plus  = binOp Num  (+)
equal = binOp Bool (==)
cond  = Fun (\b -> Fun (\x -> Fun (\y -> if prjBool b then x else y)))

prims :: Env
prims = [ ("+",plus),
          ("*", times), 
          ("-", minus), 
          ("==", equal), 
          ("if", cond), 
          ("True", Bool True),
          ("False", Bool False)]


--- | Simple parser for turning lambda expressions (plus operators and numbers, into terms)

type ParseVal = Either String Value
type ParseRet = Parser (Either String Term)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf ";:,./\'~!@#$%^&*><?|-+="

parseLit :: Parser Term
parseLit = do
            num  <- many1 digit
            return $ case reads num of
                            ((n,""):_) -> Lit n

parseId  :: Parser Term
parseId  = do
             ls <- many1 (letter <|> digit) <|> parens (many1 symbol)
             return $ case ls of
                       ('(':ls) -> Use (init ls)
                       _        -> Use ls

parseName :: Parser Term
parseName = do
             f  <- letter <|> symbol 
             ls <- many (letter <|> digit <|> symbol) 
             return $ Use (f:ls)

--- | Parse between parentheses
parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

--- | Automatic currying of an anonymous function
--- | Syntax is identical to haskell:
--- | \p0 p1 p2 p3 -> f p0 p2 (p1 p3)    -- <-- Anonymous function that takes some inputs and maps them etc
--- | (\x -> x)     -- <-- The identity function
--- | (\y f -> f (y f))  -- <-- The y-combinator
parseFun :: Parser Term
parseFun = do
            skipMany space
            char '\\'
            skipMany space
            f     <- parseId
            spaces
            fs    <- parseId `sepEndBy` spaces
            _     <- skipMany space >> string "->" >> skipMany space 
            body  <- parseTerm
            return $ foldr (Abs . (\(Use x) -> x)) (Abs ((\(Use x) -> x) $ last (f:fs)) body) $ init (f:fs)


--- | FUNCTION APPLICATION! :D 
--- | Syntax is... Whitespace! Or parentheses. f x == f (x) == f(x)
parseApp :: Parser Term
parseApp = do
            fun   <- skipMany space >> (parseName <|> parens (parseApp <|> parseFun))
            lhs   <- many app
            --- | Function application is left associative
            return $ case lhs of
                      (h:hs) -> foldl App (App fun h) hs 
                      []     -> fun
            where
            app = (spaces >> (parseLit <|> parseName <|> parens parseTerm)) <|> parens parseTerm

--- | Name a function with let <name> = <expr> syntax
parseLambda :: Parser Term
parseLambda = letexp <|> parseTerm
          where 
          letexp = do
                    skipMany space 
                    string "let" 
                    (Use name) <- spaces >> parseId
                    skipMany space
                    char '='
                    skipMany space
                    body <- parseTerm
                    return $ Rec name body
            

parseTerm :: Parser Term
parseTerm = parseFun <|> parseLit <|> parseApp <|> parens parseTerm

doTerm :: Env -> String -> ParseVal
doTerm env s = case parse parseLambda "" s of
            Left err  -> Left $ unwords . lines $ show err
            Right val -> case val of
                            (Rec n _) -> Right $ Let n (eval env val)
                            _         -> Right $ eval env val


--- | REPL
--- | Example:
--- | λ> let f = \n -> if (== 0 n) 1 (* n (f (- x 1)))
--- | λ> f 4
--- | 24
main :: IO ()
main = hSetBuffering stdout NoBuffering >> runEnv []
     where
     runEnv env = putStr "λ> " >>
                  getLine >>= \str -> 
                   case str of
                    (':':'l':' ':fn) -> readFile (trim fn) >>= \c -> runEnv . procEnv env . lines $ c
                    _                -> case doTerm env (trim str) of
                                            Left err -> putStrLn err >> runEnv env
                                            Right ret -> case ret of
                                                        (Let n f) -> runEnv $ define n f env
                                                        _         -> print ret >> runEnv env
                where 
                procEnv env []     = env
                procEnv env (x:xs) = case (doTerm env . trim) x of
                                        Right (Let n f) -> procEnv (define n f env) xs
                                        Right _         -> procEnv env xs
                                                                  

dropFromTailWhile _ [] = []
dropFromTailWhile p items
    | p (last items) = dropFromTailWhile p $ init items
    | otherwise      = items
    
trim :: String -> String
trim = dropFromTailWhile isSpace . dropWhile isSpace                                

-- a term representing factorial and a "wrapper" for evaluation
--- | Factorial omitted; see link at top
