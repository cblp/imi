{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Parallel.Strategies
-- import Criterion.Measurement
import System.TimeIt

f :: Double -> Double
f x = sqrt (1 - x * x)

integrate :: Double -> Double -> Int -> Double
integrate a b n =
    sum [f (dx * 0.5 + dx * fromIntegral i) | i <- [0 .. n - 1]]
    * dx
  where
    dx = (b - a) / fromIntegral n

integrateP :: Double -> Double -> Int -> Double
integrateP a b n =
    let
    !ps = [f (dx * 0.5 + dx * fromIntegral i) | i <- [0 .. n - 1]] `using` parList rdeepseq
    in
    sum ps
    * dx
  where
    dx = (b - a) / fromIntegral n

main :: IO ()
main = do
    -- initializeTime
    timeIt (print (4 * integrate  0 1 n))
    timeIt (print (4 * integrateP 0 1 n))
  where
    n = 1000000
