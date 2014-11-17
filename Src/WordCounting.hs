module Src.WordCounting (wordCountMapReduce, wordCountMapReducePar) where

	import Src.MapReduce
	import Src.MapReducePar
	import Data.Map (Map)
	import Control.Parallel.Strategies
	import Src.MapReduceMonad

	wordCountMap :: a -> String -> [(String, Int)]
	wordCountMap = const (map (flip (,) 1) . words)

	wordCountReduce :: a -> [Int] -> Maybe Int
	wordCountReduce = const (Just . sum)

	wordCountMapReduce :: Map k1 String -> Map String Int
	wordCountMapReduce = mapReduce wordCountMap wordCountReduce

	wordCountMapReducePar :: Map k1 String -> Map String Int
	wordCountMapReducePar = mapReducePar wordCountMap wordCountReduce