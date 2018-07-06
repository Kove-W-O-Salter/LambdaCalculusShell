
--
-- Copyright (c) Kove W. Ochre-Salter, 4th of July, 2018.
-- A simple implementation of the LambdaCalculus library.
--

module Main where
    --
    -- For Yamp.
    --
    import Data.Yamp
    import Text.Yamp
    import Control.Applicative

    --
    -- For LambdaCalculus.
    --
    import LambdaCalculus

    --
    -- For hFlush.
    --
    import System.IO

    --
    -- For Haskeline.
    --
    import System.Console.Haskeline

    --
    -- The EntryPoint.
    --
    main :: IO ()
    main  = do runInputT defaultSettings repl

    --
    -- The REPL.
    --
    repl :: InputT IO ()
    repl  = do ml <- getInputLine "λ>>> "
               case ml of
                    Nothing      -> return ()
                    Just    ":q" -> return ()
                    Just    l    ->
                        do let r = parseRun l
                           outputStrLn $ " =>>> " ++ r
                           repl

    --
    -- Parse and run λ-calculus.
    --
    parseRun   :: String -> String
    parseRun s  = case parsePedantic expr s of
        []  -> "[Syntax Error]"
        [e] ->
            case run e of
                []  -> "[Semantics Error]"
                [r] -> show r

    --
    -- Parse an expression.
    --
    expr :: Parser Expr
    expr  =  pxpr
         <|> vari
         <|> abst
         <|> appl

    --
    -- Parse an expression in parenthesis.
    --
    pxpr :: Parser Expr
    pxpr  = sandwhich (matchChar '(') (matchChar ')') expr

    --
    -- Parse a variable.
    --
    vari :: Parser Expr
    vari  = do v <- var
               return $ Var v

    --
    -- Match a variable name.
    --
    var :: Parser String
    var  = do x <- lower
              xs <- many (letter <|> digit)
              return (x:xs)

    --
    -- Parse an abstraction.
    --
    abst :: Parser Expr
    abst  = do matchChar '\\' <|> matchChar 'λ'
               spaces
               v <- var
               spaces
               matchChar '.'
               spaces
               e <- expr
               return $ Abs v e

    --
    -- Parse an application.
    --
    appl :: Parser Expr
    appl  = do matchChar '{'
               spaces
               e1 <- expr
               spaces1
               e2 <- expr
               spaces
               matchChar '}'
               return $ App e1 e2
