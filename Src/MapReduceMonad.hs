module Src.MapReduceMonad where

	import Data.List (nub)
	import Control.Applicative ((<$>))
	import Control.Monad (liftM)
	import Control.DeepSeq (NFData)
	import Prelude hiding (return,(>>=))
	import Src.Hashable (Hashable, hash)
	import qualified Src.ParallelMap as P

	class Monad' m where
		return :: a -> m s x s a
		(>>=)  :: (Eq b, NFData s'', NFData c) => 
				  m s a s' b ->
				  (b -> m s' b s'' c) ->
				  m s a s'' c

	newtype MapReduce s a s' b = MR { runMR :: ([(s,a)] -> [(s',b)]) }

	instance Monad' MapReduce where
	 	return = retMR
	 	(>>=)  = bindMR

	retMR :: a -> MapReduce s x s a
	retMR k = MR (\ss -> [(s,k) | s <- fst <$> ss])

	bindMR :: (Eq b, NFData s'', NFData c) =>
			  MapReduce s a s' b ->
			  (b -> MapReduce s' b s'' c) ->
			  MapReduce s a s'' c
	bindMR f g = MR (\s ->
		let
			fs = runMR f s
			gs = map g $ nub $ snd <$> fs
		in
		concat $ P.map (\g' -> runMR g' fs) gs)

	runMapReduce :: MapReduce s () s' b -> [s] -> [(s',b)]
	runMapReduce m ss = (runMR m) [(s,()) | s <- ss]

	distributMR :: (Hashable s) => MapReduce s () s Int
	distributMR = MR (\ss -> [(s, hash s) | s <- fst <$> ss])

	wrapMR f = (\k -> MR (g k))
		where
			g k ss = f $ fst <$> filter (\s -> k == snd s) ss
