{-# LANGUAGE ForeignFunctionInterface #-}
module Main
  (
    main
  ) where
import System.Console.ANSI
import System.Process
import System.IO
import System.Exit
import Control.Monad
import Data.Char(ord)
import LexInfo
import DebugInfo
resetScreen = do
  clearScreen
  cursorUpLine 1000
  return ()
isWordChar :: Char -> Bool
isWordChar ch = ch `elem` wordCharList
  where wordCharList = '_':['A'..'Z']++['a'..'z']
{- 'A' == UpArrow; 'B' == DownArrow;
   'C' == RightArrow; 'D' == LeftArrow-}
dealArrow :: Char -> IO()
dealArrow ch
  | ch == 'A' = cursorBackward 4 >> clearFromCursorToLineEnd >> cursorUpLine 1
  | ch == 'B' = cursorBackward 4 >> clearFromCursorToLineEnd >> cursorDownLine 1
  | ch == 'C' = cursorBackward 4 >> clearFromCursorToLineEnd >> cursorForward 1
  | ch == 'D' = cursorBackward 4 >> clearFromCursorToLineEnd >> cursorBackward 1
  | otherwise = return ()
cmdParser :: Char -> IO String
cmdParser ch
  | ch == 'q' = cursorBackward 1 >> clearFromCursorToLineEnd >> return "quit"
  | ch == 's' = cursorBackward 1 >> clearFromCursorToLineEnd >> return "savefile"
  | otherwise = cursorBackward 1 >> return "undefined"
getWord :: String -> IO String
getWord str = getChar >>= go str where
  go str ch = if ch == '\DEL'
                 then do
                   cursorBackward 3
                   clearFromCursorToLineEnd
                   getWord (drop 1 str)
                 else if ch == '\ESC' -- ^[[A == UpArrow
                   then do
                     getChar >> getChar >>= dealArrow
                     return $reverse str
                 else if ch == ':'
                   then
                   cursorBackward 1 >> clearFromCursorToLineEnd >> getChar >>= cmdParser
                 else if not (isWordChar ch)
                   then do
                     return $reverse (str)
                 else
                   getWord $ch:str
{- type 0 : normal word
   type 1 : keyword-}
parse :: String -> IO Token
parse str
  | str `elem` keywords  = do
    setSGR [SetColor Foreground Vivid Red] >> cursorBackward (length str + 1)
    putStr str >> putChar ' '
    setSGR [Reset]
    return (Token str 1)
  | str == "quit" = return (Token "" (-1))
  | str `elem` whiteSpace = return Empty
  | otherwise = return (Token str 0)
  where keywords = getKeyRsv 'c'
        whiteSpace = [" ", "\n", "\t"]
saveToken :: Handle -> Token -> IO (Token)
saveToken tmph token = if tokenType token == -1
                          then
                          hClose tmph >> exitSuccess
                          else do
                            hPutStr tmph $tokenValue token
                            return token
getToken :: IO Token
getToken = getWord "" >>= parse
main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  resetScreen
  forever $do
    let tokenList = [] in
      mainloop tokenList
        where mainloop tokenList = getToken >>= \s -> return (s:tokenList) >>= showInfo False >>= mainloop
