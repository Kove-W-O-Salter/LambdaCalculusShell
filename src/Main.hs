
{-
 - Copyright (c) Kove W. Ochre-Salter, 4th of July, 2018.
 - A simple implementation of the LambdaCalculus library.
 -}

module Main where

  import LambdaCalculus
  import System.Console.Readline
  import Text.ParserCombinators.Parsec

  main :: IO ()
  main  = do l <- readline "Lcs Repl> "
             case l of
              Nothing      -> main
              Just ":quit" -> return ()
              Just s       -> do addHistory s
                                 let r = parseRun s
                                 putStrLn r
                                 main

  parseRun     :: String -> String
  parseRun src  = case parse expr "Lcs" src of
    Left  er -> show er
    Right ex -> show $ run ex

  expr :: Parser Expr
  expr  = try var <|> try abst <|> try app <|> try pexpr <?> "Expression"

  var :: Parser Expr
  var  = do l <- varName
            return $ Var l

  abst :: Parser Expr
  abst  = do char '\\'
             spaces
             v <- varName
             spaces
             char '.'
             spaces
             e <- expr
             return $ Abs v e

  app :: Parser Expr
  app  = do e1 <- expr
            spaces
            e2 <- expr
            return $ App e1 e2

  pexpr :: Parser Expr
  pexpr  = between (char '(') (char ')') expr

  varName :: Parser String
  varName  = do x <- lower
                xs <- many (letter <|> digit)
                return $ x : xs
