module SimHash where

import Data.Bits
import Data.Foldable
import Data.BloomFilter.Hash (Hashable,hash64)
import Data.Word

newtype FeatureVector a = FV [a] deriving Show

computeHash :: Hashable a => FeatureVector a -> Word64
computeHash (FV fv) = foldl' (\x (b,v) -> sign v x b) 0
                    $ zip b_range weightVector
  where
    -- Hash of each individual feature
    hsh  = map hash64 fv
    -- Weight vector per hash
    bits = map (\x -> map (\z-> if testBit x z then 1 else (- 1)) b_range) hsh
    -- Summed Weight vector
    weightVector = foldr1 (zipWith (+)) bits
    -- Helper function to set the bits
    sign v = if v < 0 then clearBit else setBit

hammingDistance :: Word64 -> Word64 -> Int
hammingDistance a b = h_dist 0 (a `xor` b)
  where
    h_dist c v 
      | v == 0    = c
      | otherwise = h_dist (c + 1) (v .&. (v - 1))

  

b_range :: [Int]
b_range = [0 .. 63]
