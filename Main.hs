module Main where

	import Src.WordCounting (wordCountMapReduce, wordCountMapReducePar)
	import Data.Map (empty, insert, toList, fromList)
	import Criterion.Measurement

	main :: IO()
	main = do
		contents <- readFile "./TextFiles/test.txt"
		contentsDouble <- readFile "./TextFiles/test.txt"
		let docMap = insert "doc2" contentsDouble $ insert "doc" contents $ empty
		start <- getCPUTime
		let result = wordCountMapReducePar docMap
		end <- getCPUTime
		--mapM_ putStrLn $ map show $ toList result
		putStrLn $ "Total time for map reduce: " ++ show (end - start)
