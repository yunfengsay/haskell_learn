module Lib
    ( someFunc
    ) where

import System.Directory

someFunc :: IO ()
someFunc = do
	filesPath <- getDirectoryContents "./text/Naive_Bayes/SogouC/all"
	mapM_ putStrLn filesPath

