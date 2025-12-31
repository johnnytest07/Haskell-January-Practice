module Parser where

import Types
import Lexer
import Examples

import Data.Maybe

------------------------------------------------------------------------------
-- Given...

showToken :: Token -> String
showToken (Ident v) = v
showToken (Nat v) = show v
showToken WhileTok = "while"
showToken t = [head [c | (c, t') <- tokenTable, t == t']]

printParse :: String -> IO ()
printParse input = either printError printOK (parse input)
  where
    printOK prog = putStrLn "Parse successful..." >> print prog
    printError err = putStr "Parse error: " >> printError' err
    printError'' t s = putStrLn (s ++ " expected, but " ++
                                 maybe "nothing" showToken t ++ " found")
    printError' (BadChar c) = do putStr "Unrecognised character: "
                                 putStrLn [c]
    printError' (Unexpected t t') = printError'' t (showToken t')
    printError' (StmtNotFound t) = printError'' t "Statement"
    printError' (ExprNotFound t) = printError'' t "Expression"
    printError' (IntNotFound t) = printError'' t "Integer literal"
    printError' (UnparsedInput toks) = putStrLn ("Unparsed input: " ++
                                                 unwords (map showToken toks))

------------------------------------------------------------------------------

-- Given...
mHead :: [a] -> Maybe a
mHead (x : _) = Just x
mHead _ = Nothing

checkTok :: Token -> [Token] -> Either Error [Token]
checkTok ref reflst
  |Just ref == mHead reflst = Right (tail reflst)
  |otherwise = Left (Unexpected (mHead reflst) ref)

parseAtom :: Parser Expr
parseAtom ((Nat x):xs) = Right (xs, Val x)
parseAtom (Minus:(Nat x):xs) = Right (xs, Val (negate x))
parseAtom (Minus:x:xs) = Left (IntNotFound (Just x))
parseAtom ((Ident x):xs) = Right (xs, Var x)
parseAtom x = Left (ExprNotFound (mHead x))

parseTerm :: Parser Expr
parseTerm = parseSth parseAtom Times Mul

parseSth :: Parser Expr -> Token -> (Expr -> Expr -> Expr) -> Parser Expr
parseSth fun op constr toks = do
  (toks1, x0) <- fun toks
  parseSth' fun op constr x0 toks1

parseSth' :: Parser Expr -> Token -> (Expr -> Expr -> Expr)
          -> Expr -> Parser Expr
parseSth' fun op constr acc (t : toks)
  | t == op = do
      (toks1, x1) <- fun toks
      parseSth' fun op constr (constr acc x1) toks1
parseSth' _ _ _ acc toks =
  Right (toks, acc)

parseExpr :: Parser Expr
parseExpr = parseSth parseTerm Plus Add

parseStmt :: Parser Stmt
parseStmt ((Ident x):Eq:(Nat val):xs) = Right (xs, Asgn x (Val val))
parseStmt ((Ident x):Eq:(Ident y):xs) = Right (xs, Asgn x (Var y))
parseStmt ((Ident x):Eq:rest) = Left (ExprNotFound (mHead rest))
parseStmt ((Ident x):rest) = Left (Unexpected (mHead rest) Eq)
parseStmt whole@(_:xs) = Left (StmtNotFound (mHead whole))



parseBlock :: Parser Block
parseBlock = undefined

parse :: String -> Either Error Program
parse input = undefined