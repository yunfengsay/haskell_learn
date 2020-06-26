module DebugInfo
  (
    showInfo
  ) where
import LexInfo
showInfo :: Bool -> [Token] -> IO [Token]
showInfo flag tokenList = case flag of
                            True -> do
                              print tokenList >> return tokenList
                            otherwise -> return tokenList
