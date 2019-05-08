module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

parseStringEscapeSequence :: Parser Char
parseStringEscapeSequence = do
    char '\\'
    character <- oneOf "\"nrt\\"
    return $ case character of
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _ -> character

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"\\" <|> parseStringEscapeSequence)
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

data Radix = Octal | Decimal | Hexadecimal

parseNumberRadix :: Parser Radix
parseNumberRadix = do
    char '#'
    character <- oneOf "odx"
    return $ case character of
        'o' -> Octal
        'd' -> Decimal
        'x' -> Hexadecimal

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
-- parseNumber = (many1 digit) >>= (return . Number . read)
parseNumber = do
    radix <- parseNumberRadix <|> (return Decimal)
    digits <- many1 digit
    let (number,_):_ = case radix of
            Octal -> readOct digits
            Decimal -> readDec digits
            Hexadecimal -> readHex digits
    let lispVal = Number number
    return lispVal

parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do
        char '('
        x <- try parseList <|> parseDottedList
        char ')'
        return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
