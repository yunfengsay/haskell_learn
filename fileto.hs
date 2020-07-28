import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
import System.Process
import System.Cmd

main = do
	args <- getArgs
	mapM putStrLn args
	(command:args) <- getArgs
	let (Just action) = lookup command dispatch
	action args

dispatch :: [(String, [String] -> IO())]
dispatch = [("add", add)
			,("view", view)
		   ,("edit", edit)
		   ]

add [fileName] = do
	fileExist <- doesFileExist fileName
	if not fileExist
	then do
		system $ "touch " ++  fileName
		return ()
	else return () 
	putStrLn "done"

view [fileName] = do
	contents <- readFile fileName
	let numberdContents = zipWith (\n line -> show n ++ "-" ++ line) [0..] $ lines contents
	putStr $ unlines numberdContents

edit [fileName] = do
	return ()
