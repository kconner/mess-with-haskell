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

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- typeclass instancing == protocol conformance
instance Show LispVal where show = showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in
    if null parsed
        then 0
        else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)]

-- maybe x y == y ?? x
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
