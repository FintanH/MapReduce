{-# LANGUAGE FlexibleInstances #-}

module Src.Hashable (Hashable, hash) where

	import qualified Crypto.Hash.MD5 as H
	import Data.Char (ord)
	import Data.ByteString (ByteString, pack , unpack)

	class Hashable s where
		conv :: s -> ByteString

	hash :: (Hashable s) => s -> Int
	hash s = sum $ map fromIntegral (unpack h)
		where
			h = H.hash $ conv s

	instance Hashable String where
		conv s = pack $ map (fromIntegral . ord) s 