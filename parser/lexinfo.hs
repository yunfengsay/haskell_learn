module LexInfo
  (
    Token(..),
    getKeyRsv
  ) where
data Token = Token {
  tokenValue :: String,
  tokenType :: Int
                   } | Empty deriving (Show)
getKeyRsv :: Char -> [String]
getKeyRsv ch
  | ch == 'c' =
    ["auto", "break", "case", "char", "const", "continue", "default", "do", "double", "else", "enum", "extern", "float", "for", "goto", "if", "int", "long", "register", "return", "short", "signed", "sizeof", "static", "struct", "switch", "typedef", "union", "unsigned", "void", "volatile", "while"]
  | otherwise = [""]
