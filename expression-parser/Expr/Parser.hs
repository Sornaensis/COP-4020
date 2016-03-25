module Expr.Parser (parseExpr) where

import Text.ParserCombinators.Parsec
import Expr.Expression (Expr(..))

tryParsers :: [Parser a] -> Parser a
tryParsers [] = undefined
tryParsers xs = foldr1 (<|>) . map try $ xs

spaced :: Parser a -> Parser a
spaced = between (many space) (many space) 

infixExp :: Parser Expr -> [(String, Expr -> Expr -> Expr)] -> Parser Expr
infixExp next [] = undefined
infixExp next ps = do t1 <- next
                      t2 <- spaced . option "" . tryParsers $ map (string . fst) ps
                      case lookup t2 ps of
                        Nothing -> return t1
                        Just cons -> do follow <- next
                                        many space
                                        return $ cons t1 follow

equalTerm :: Parser Expr
equalTerm = infixExp orTerm [("==",Eq),("!=",Neq)]

orTerm :: Parser Expr
orTerm = do t1 <- andTerm
            next <- spaced . option "" . tryParsers $ [string "||"]
            case next of
                [] -> return t1
                "||" -> do follow <- spaced orTerm
                           return $ Or t1 follow

andTerm :: Parser Expr
andTerm = do t1 <- arithTerm
             next <- spaced . option "" . tryParsers $ [string "&&"]
             case next of
                 [] -> return t1
                 "&&" -> do follow <- spaced andTerm
                            return $ And t1 follow



arithTerm :: Parser Expr
arithTerm = do t1 <- facTerm
               next <- spaced . option "" . tryParsers $ [string "+" ,string "-"]
               case next of
                 []   -> return t1
                 "+"  -> do follow <- spaced arithTerm
                            return $ foldSubAdd (Add t1 follow)
                 "-"  -> do follow <- spaced arithTerm
                            return $ foldSubAdd (Sub t1 follow)

--- | Fold consecutive addition/subtraction operations to make them left-associative
--- | e.g. -- 5-4-3-2-1 == (((5-4)-3)-2)-1
--- | e.g. -- 5-4-3-2-1 /= 5-(4-(3-(2-1)))
--- | Without this the parser will produce right-associative expressions only
foldSubAdd (Add e0 (Add e1 e2))  = Add (foldSubAdd $ Add e0 e1) $ foldSubAdd e2
foldSubAdd (Sub e0 (Sub e1 e2))  = Sub (foldSubAdd $ Sub e0 e1) $ foldSubAdd e2
foldSubAdd (Add e0 (Sub e1 e2))  = Sub (foldSubAdd $ Add e0 e1) $ foldSubAdd e2
foldSubAdd (Sub e0 (Add e1 e2))  = Add (foldSubAdd $ Sub e0 e1) $ foldSubAdd e2
foldSubAdd e                     = e

--- | Same as «foldSubAdd» but for Multiplication/Division
foldMulDiv (Mul e0 (Mul e1 e2)) = Mul (foldMulDiv $ Mul e0 e1) $ foldMulDiv e2
foldMulDiv (Div e0 (Div e1 e2)) = Div (foldMulDiv $ Div e0 e1) $ foldMulDiv e2
foldMulDiv (Mul e0 (Div e1 e2)) = Div (foldMulDiv $ Mul e0 e1) $ foldMulDiv e2
foldMulDiv (Div e0 (Mul e1 e2)) = Mul (foldMulDiv $ Div e0 e1) $ foldMulDiv e2
foldMulDiv e                    = e

facTerm :: Parser Expr
facTerm = do t1 <- prodTerm
             next <- spaced . option "" . tryParsers $ [string "*", string "/"]
             case next of
               []   -> return t1
               "*"  -> do follow <- spaced facTerm
                          return $ foldMulDiv $ Mul t1 follow
               "/"  -> do follow <- spaced facTerm
                          return $ foldMulDiv $ Div t1 follow


prodTerm :: Parser Expr
prodTerm = do t1 <- expTerm
              next <- spaced . option "" . tryParsers $ [string "^", string "**"]
              case next of
                []   -> return t1
                _    -> do follow <- prodTerm
                           many space
                           return $ Exp t1 follow

expTerm :: Parser Expr
expTerm = do prefix <- spaced . option "" . tryParsers $ [string "++", string "--", string "+",  string "-",  string "!"]
             body   <- spaced unarTerm
             return $ case prefix of
                         []   -> body
                         "!"  -> Not body
                         "+"  -> Id body
                         "-"  -> Neg body
                         "++" -> Inc body
                         "--" -> Dec body

unarTerm :: Parser Expr
unarTerm = do e <- boolean <|> number <|> between (char '(') (char ')') equalTerm
              return $ case e of
                            Lit _   -> e
                            Bool _  -> e
                            _       -> Id e

boolean :: Parser Expr
boolean = do b <- string "True" <|> string "False"
             return $ Bool (read b :: Bool)

number :: Parser Expr
number = do num <- many1 digit
            return $ Lit (read num :: Integer)

parseExpr :: String -> Either String Expr
parseExpr s = case parse equalTerm "" s of
                Left err  -> Left . unwords . lines $ show err
                Right val -> Right val

