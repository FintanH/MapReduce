module Main where

	import Src.WordCounting
	import Data.Map (empty, insert, toList, fromList, Map)
	import Criterion.Measurement
	import System.Environment
	import Control.Exception.Base

	numDocs = 10

	main :: IO()
	main = do
		f <- getArgs
		contents <- readFile "./TextFiles/test.txt"
		let docs = loop numDocs contents empty
		if f /= []
			then
				if head f == "-p"
					then
						doMapReduce wordCountMapReducePar docs "./TextFiles/TimeResultsPar.txt"
					else if head f == "-m"
						then
							mapM_ putStrLn $ map show $ wordCountMapReducePar' (concat $ replicate 10 contents)
							--print $ mapReduceMR (contents:[])
						else
							doMapReduce wordCountMapReduce docs "./TextFiles/TimeResults.txt"
			else 
				doMapReduce wordCountMapReduce docs "./TextFiles/TimeResults.txt"

		where
			doMapReduce :: (Map k1 String -> Map String Int) -> Map k1 String -> FilePath -> IO ()
			doMapReduce mapReduce docs file = do
				start <- getCPUTime
				let result = mapReduce docs
				end <- getCPUTime
				appendFile file $ details (end - start)
				--evaluate result -- force evaluate result
				--return ()
				mapM_ putStrLn $ map show $ toList result

			--creat n documents
			loop :: Int -> String -> Map [Char] String -> Map [Char] String
			loop 0 contents m = m
			loop n contents m = let m' = insert ("doc" ++ show n) contents m in loop (n - 1) contents m'

			--stringify time to place in document
			details :: Double -> String
			details time = show numDocs ++ " " ++ show time ++ "\n"
