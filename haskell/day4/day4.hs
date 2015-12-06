{- Requires pureMD5 pacakge -}
{-
--- Day 4: The Ideal Stocking Stuffer ---

Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically forward-thinking little girls and boys.
To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. The input to the MD5 hash is some secret key (your puzzle input, given below) followed by a number in decimal. To mine AdventCoins, you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.

For example:
    If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so.
    If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....
-}

import Data.Digest.Pure.MD5
import Data.ByteString.Lazy.Char8
import Data.List

-- Get the lengh of the list of hashes that ends when the condition of starting with 5 zeros is met
partOne key = Prelude.length $ getHashesUntil (\xs -> not $ Data.List.isPrefixOf "00000" xs) key

getHashesUntil func key = Prelude.takeWhile func (genHashes key 0)

-- Generate A list of hashes based on a key and an index
-- The input to the hash is key ++ index :: string
genHashes key index = show (md5 $ pack (key ++ show index)) : genHashes key (index + 1)


{-
--- Part Two ---

Now find one that starts with six zeroes.
-}

partTwo key = Prelude.length $ getHashesUntil (\xs -> not $ Data.List.isPrefixOf "000000" xs) key
