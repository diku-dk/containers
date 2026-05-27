-- This is a script for generating constants for the
-- n-dimensional linear congruential generator.

import Control.Monad
import Data.List
import Data.Word
import System.Random

type IntArray = [Word]

randInt :: Int -> Int -> IO IntArray
randInt bits exp = do
  result <- padInt bits . reverse <$> revRandInt exp
  if any (/= 0) result
    then pure result
    else randInt bits exp
  where
    revRandInt i =
      if i <= 64
        then singleton . (`mod` m) <$> (randomIO :: IO Word)
        else (:) <$> (randomIO :: IO Word) <*> revRandInt (i - 64)
      where
        m = 2 ^ i - 1 :: Word

    padInt bits i = (replicate pad 0) ++ i
      where
        j = bits `div` 64
        pad = j - length i

printInt :: Int -> IntArray -> String
printInt bits =
  (++ ("] :> [" ++ str_bits ++ ".n]u64)"))
    . ((str_bits ++ ".from_u64 ([") ++)
    . intercalate ", "
    . map ((++ "u64") . show)
  where
    str_bits = "u" ++ show bits

randNInt :: Int -> Int -> Int -> IO [IntArray]
randNInt n bits exp = replicateM n (randInt bits exp)

randMatInt :: Int -> Int -> Int -> IO [[IntArray]]
randMatInt n bits exp = replicateM n (randNInt (n + 1) bits exp)

printInts :: Int -> [IntArray] -> String
printInts bits =
  ("[" ++)
    . (++ "]")
    . intercalate "\n  ,"
    . map (printInt bits)

printMatrix :: Int -> [[IntArray]] -> String
printMatrix bits =
  ("[" ++)
    . (++ "]")
    . intercalate "\n,"
    . map (printInts bits)

matrix :: Int -> Int -> Int -> IO String
matrix n bits exp = do
  mat <- randMatInt n bits exp
  let str = printMatrix bits mat
  pure $ str ++ " :> [n][n + 1]u" ++ show bits
