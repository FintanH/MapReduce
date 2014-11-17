module Src.ParallelMap (map) where

import Control.Parallel.Strategies (parMap, rdeepseq)
import Control.DeepSeq (NFData)
import Prelude hiding (map)

map :: (NFData b) => (a -> b) -> [a] -> [b]
map = parMap rdeepseq