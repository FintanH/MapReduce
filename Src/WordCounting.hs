module Src.WordCounting (wordCountMapReduce, wordCountMapReducePar, mapReduceMR, wordCountMapReducePar') where

	import Src.MapReduce
	import Src.MapReducePar
	import Data.Map (Map)
	import Control.Parallel.Strategies
	import Src.MapReduceMonad
	import Prelude hiding ((>>=))
	import Data.List
	import Data.Function

	wordCountMap :: a -> String -> [(String, Int)]
	wordCountMap = const (map (flip (,) 1) . words)

	wordCountReduce :: a -> [Int] -> Maybe Int
	wordCountReduce = const (Just . sum)

	wordCountMapReduce :: Map k1 String -> Map String Int
	wordCountMapReduce = mapReduce wordCountMap wordCountReduce

	wordCountMapReducePar :: Map k1 String -> Map String Int
	wordCountMapReducePar = mapReducePar wordCountMap wordCountReduce

	wordCountMap' :: String -> (String, Int)
	wordCountMap' = (flip (,) 1)

	wordCountReduce' :: [(String,Int)] -> [(String,Int)]
	wordCountReduce' = groupKey . sortBy (compare `on` fst)
		where
			groupKey :: [(String,Int)] -> [(String,Int)]
			groupKey (x:[]) = [x]
			groupKey (x:y:xs) = if fst x == fst y 
									then let 
											(k,v) = (fst x, snd y + snd x) in groupKey ((k,v) : xs)
									else x : groupKey (y:xs)

	wordCountMapReducePar' :: String -> [(String,Int)]
	wordCountMapReducePar' = mr wordCountMap' wordCountReduce' . words

	mapReduceMR :: [String] -> [(String,Int)]
	mapReduceMR state = runMapReduce mr state
		where
			mr = distributeMR >>= wrapMR mapper >>= wrapMR reducer

	mapper :: [String] -> [(String,String)]
	mapper [] = []
	mapper (x:xs) = parse x ++ mapper xs
		where
			parse x = map (\w -> (w,w)) $ words x

	reducer :: [String] -> [(String,Int)]
	reducer [] = []
	reducer xs = [(head xs, length xs)]