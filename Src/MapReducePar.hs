{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -XExplicitForAll #-}
module Src.MapReducePar (mapReducePar) where

	import Data.Map (Map,empty,insertWith,mapWithKey,filterWithKey,toList,fromList,traverseWithKey,delete)
	import Control.Parallel.Strategies (NFData,parList,parMap,withStrategy,rdeepseq,rparWith,rpar,rseq,runEval)
	import Control.Monad.Par (new,fork,put,get,IVar,Par,runPar)
	import Data.Traversable (traverse)

	mapReducePar :: forall k1 k2 v1 v2 v3. 
				 (Ord k2, NFData k2, NFData v2, NFData v3)
			=> (k1 -> v1 -> [(k2,v2)])
			-> (k2 -> [v2] -> Maybe v3)
			-> Map k1 v1
			-> Map k2 v3

	mapReducePar mAP rEDUCE = 
		  reducePerKeyPar
		. groupByKeyPar
		. mapPerKeyPar

		where

			mapPerKeyPar :: Map k1 v1 -> [(k2,v2)]
			mapPerKeyPar =
			  parConcat' . parMap rdeepseq (uncurry mAP) . toList
			  	where
			  		parConcat' = runEval . parConcat (rparWith rseq)
			  		parConcat strat [] = return []
			  		parConcat strat (x:xs) = do
			  			x' <- strat x
			  			xs' <- parConcat strat xs
			  			return (x' ++ xs')

			groupByKeyPar :: [(k2,v2)] -> Map k2 [v2]
			groupByKeyPar = foldl insert empty
				where
					insert dict (k2,v2) = insertWith (++) k2 [v2] dict

			reducePerKeyPar :: Map k2 [v2] -> Map k2 v3
			reducePerKeyPar = 
				mapWithKeyPar unJust 
			  . filterWithKey isJust
			  . mapWithKeyPar rEDUCE
				where
					isJust k (Just v) = True
					isJust k Nothing  = False
					unJust k (Just v) = v

			spawn :: NFData a => Par a -> Par (IVar a)
			spawn p = do
				i <- new
				fork (do x <- p; put i x)
				return i

			mapWithKeyPar :: NFData b => (k -> a -> b) -> Map k a -> Map k b 
			mapWithKeyPar f m = runPar $ do
				_m <- traverseWithKey (\i jmap -> spawn (return (f i jmap))) m
				traverse get _m
