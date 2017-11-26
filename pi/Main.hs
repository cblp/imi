module Main where

import Control.Parallel.Strategies

f :: Double -> Double
f x = sqrt (1 - x * x)

integrate :: Double -> Double -> Int -> Double
integrate a b n =
    sum ([f (dx * 0.5 + dx * fromIntegral i) | i <- [0 .. n - 1]] `using` parList rdeepseq)
    * dx
  where
    dx = (b - a) / fromIntegral n

main :: IO ()
main =
    print (4 * integrate 0 1 1000000)
