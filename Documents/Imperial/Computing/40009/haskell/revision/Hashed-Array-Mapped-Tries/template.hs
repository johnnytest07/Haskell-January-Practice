module Tries where

import Data.List hiding (insert)
import Data.Bits

import Types
import HashFunctions
import Examples

--------------------------------------------------------------------
-- Part I

-- Use this if you're counting the number of 1s in every
-- four-bit block...
bitTable :: [Int]
bitTable
  = [0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4]

countOnes :: Int -> Int
countOnes inp
  = length (filter (=='1') (showBitVector inp 64))

countOnesFrom :: Int -> Int -> Int
countOnesFrom i n
  = countOnes ((bit i - 1) .&. n)

getIndex :: Int -> Int -> Int -> Int
getIndex inp n b
  = shiftR (((bit (b*(n+1)) - 1) - (bit (n*b) - 1)) .&. inp) (b*n)

-- Pre: the index is less than the length of the list
replace :: Int -> [a] -> a -> [a]
replace 0 (x:xs) r = r:xs
replace n (x:xs) r = x: replace (n-1) xs r

-- Pre: the index is less than or equal to the length of the list
insertAt :: Int -> a -> [a] -> [a]
insertAt 0 r whole = r:whole
insertAt n r (x:xs) = x: insertAt (n-1) r xs
--------------------------------------------------------------------
-- Part II

sumTrie :: (Int -> Int) -> ([Int] -> Int) -> Trie -> Int
sumTrie _ funcTwo (Leaf inp) = funcTwo inp
sumTrie funcOne funcTwo (Node bitVector subnodes) = sumSubnodes subnodes
    where
        sumSubnodes :: [SubNode] -> Int
        sumSubnodes [] = 0
        sumSubnodes ((Term inp):xs) = funcOne inp + sumSubnodes xs
        sumSubnodes ((SubTrie inp):xs) = sumTrie funcOne funcTwo inp + sumSubnodes xs

-- If you get the sumTrie function above working you can uncomment
-- these three functions; they may be useful in testing.

trieSize :: Trie -> Int
trieSize t
  = sumTrie (const 1) length t

binCount :: Trie -> Int
binCount t
  = sumTrie (const 1) (const 1) t

meanBinSize :: Trie -> Double
meanBinSize t
  = fromIntegral (trieSize t) / fromIntegral (binCount t)


member :: Int -> Hash -> Trie -> Int -> Bool
member matchNo hash trie b = itrLayer 0 matchNo hash trie b
  where
    itrLayer :: Int -> Int -> Hash -> Trie -> Int -> Bool
    itrLayer layerNo match hash (Node bitVector subnodes) b
      | testBit bitVector bitPos = termOrSub layerNo match (subnodes !! arrIndex) b
      | otherwise = False
      where
        bitPos = getIndex hash layerNo b
        arrIndex = countOnesFrom bitPos bitVector
    
    itrLayer layerNo match hash (Leaf lst) b = match `elem` lst

    termOrSub :: Int -> Int -> SubNode -> Int -> Bool
    termOrSub _ match' (Term inp) _ = match' == inp
    termOrSub layerNo match' (SubTrie trie') b = 
      itrLayer (layerNo + 1) match' hash trie' b
--------------------------------------------------------------------
-- Part III

insert :: HashFun -> Int -> Int -> Int -> Trie -> Trie
insert
  = undefined

buildTrie :: HashFun -> Int -> Int -> [Int] -> Trie
buildTrie
  = undefined
