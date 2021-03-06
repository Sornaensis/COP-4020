--- | Simple parser for turning lambda expressions (plus operators and numbers, into terms)
module Lamb.Parser (ParseVal, runLambParser) where

import Control.Monad
import Lamb.Language
import Text.ParserCombinators.Parsec hiding (spaces)

-- | This is the type we return from `runLambParser`
type ParseVal = Either String Term

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
parseBool :: Parser Term
parseBool = fmap (TLit . Bool . read) $ string "True" <|> string "False"
 
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
                        f <- oneOf ['a'..'z'] 
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
            fun   <- skipMany space >> (parseName <|> parens (parseApp <|> parseFun) <|> parseBool <|> try parseFloat <|> parseInt)
            lhs   <- many app
            --- | Function application is left associative
            return $ case lhs of
                      (h:hs) -> foldl App (App fun h) hs 
                      []     -> fun
            where
            app = (skipMany space >> (try parseFloat <|> parseBool <|> parseInt <|> parseName <|> parens parseTerm)) <|> parens parseTerm

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
parseTerm = parseApp <|> parseFun <|> parseBool <|> try parseFloat <|> parseInt <|>  parens parseTerm

runLambParser :: String -> ParseVal
runLambParser s = case parse parseLambda "" s of
            Left err  -> Left $ unwords . lines $ show err
            Right val -> Right val
