--- | Simple parser for turning lambda expressions (plus operators and numbers, into terms)
module Lamb.Parser (ParseVal, doTerm) where

import Lamb.Language
import Text.ParserCombinators.Parsec hiding (spaces)

type ParseVal = Either String Value
type ParseRet = Parser (Either String Term)

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf ";:,./\'~!@#$%^&*><?|-+="

parseFloat :: Parser Term
parseFloat = do h <- many1 digit
                d <- char '.'
                l <- many1 digit
                return $ case reads (h ++ [d] ++ l) of
                            ((n,""):_) -> Lit . Real $ n

parseInt :: Parser Term
parseInt = do
            num  <- many1 digit
            return $ case reads num of
                            ((n,""):_) -> Lit . Int $ n

parseId  :: Parser Term
parseId  = do
             ls <- many1 (letter <|> digit) <|> parens (many1 symbol)
             return $ case ls of
                       ('(':ls) -> Use (init ls)
                       _        -> Use ls

parseName :: Parser Term
parseName = do
             name <- (do
                        f <- letter 
                        ls <- many (letter <|> digit)
                        return $ f:ls) <|> many1 symbol
             return $ Use name

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
            app = (skipMany space >> (try parseFloat <|> parseInt <|> parseName <|> parens parseTerm)) <|> parens parseTerm

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
parseTerm = parseFun <|> try parseFloat <|> parseInt <|> parseApp <|> parens parseTerm

doTerm :: Env -> String -> ParseVal
doTerm env s = case parse parseLambda "" s of
            Left err  -> Left $ unwords . lines $ show err
            Right val -> case val of
                            (Rec n _) -> Right $ Let n (eval env val)
                            _         -> Right $ eval env val
