{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -XExplicitForAll #-}
module Src.MapReduce (mapReduce) where

	import Data.Map (Map,empty,insertWith,mapWithKey,filterWithKey,toList)


	mapReduce :: forall k1 k2 v1 v2 v3. 
				 Ord k2
			=> (k1 -> v1 -> [(k2,v2)])
			-> (k2 -> [v2] -> Maybe v3)
			-> Map k1 v1
			-> Map k2 v3

	mapReduce mAP rEDUCE = 
		  reducePerKey 
		. groupByKey 
		. mapPerKey

		where

			mapPerKey :: Map k1 v1 -> [(k2,v2)]
			mapPerKey = 
				concat 
			  . map (uncurry mAP)
			  . toList

			groupByKey :: [(k2,v2)] -> Map k2 [v2]
			groupByKey = foldl insert empty
				where
					insert dict (k2,v2) = insertWith (++) k2 [v2] dict

			reducePerKey :: Map k2 [v2] -> Map k2 v3
			reducePerKey = 
				mapWithKey unJust 
			  . filterWithKey isJust
			  . mapWithKey rEDUCE
				where
					isJust k (Just v) = True
					isJust k Nothing  = False
					unJust k (Just v) = v
