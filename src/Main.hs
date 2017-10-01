module Main where
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool deriving (Show)

symbol :: Parser Char
symbol = oneOf "!$%&*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space

escapeChar :: Parser Char
escapeChar = do x <- anyChar
                case x of
                    '"' -> return '"'
                    'n' -> return '\n'
                    't' -> return '\t'
                    'r' -> return '\r'
                    'a' -> return '\a'
                    'b' -> return '\b'
                    'v' -> return '\v'
                    'f' -> return '\f'
                    '0' -> return '\0'
                    otherwise -> return x

stringChar :: Parser Char
stringChar = (char '\\' >> escapeChar) <|> noneOf "\""

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many stringChar
                 char '"'
                 return $ String x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                         "#t" -> Bool True
                         "#f" -> Bool False
                         otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ show val

main :: IO ()
main = do
        putStrLn "Enter expr: "
        expr <- getLine
        putStrLn (readExpr expr)
