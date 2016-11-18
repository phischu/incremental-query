module Main where

import IncrementalQuery (
  initializeCache, updateCache, Cache,
  initializeCache2, updateCache2, Cache2,
  Row(Order, LineItem), salesDomain, sales)
import Data.Monoid (
  Sum)

import Criterion.Main (
  defaultMain, bgroup, bench, whnf, nf, Benchmark, env)

main :: IO ()
main = defaultMain [
  bgroup "initializeCache" (map benchmarkInitializeCache [10, 100]),
  bgroup "updateCache" (map benchmarkUpdateCache [10, 100]),
  bgroup "initializeCache2" (map benchmarkInitializeCache2 [10, 100, 1000]),
  bgroup "updateCache2" (map benchmarkUpdateCache2 [10, 100, 1000])]

benchmarkInitializeCache :: Int -> Benchmark
benchmarkInitializeCache size =
  bench (show size) (nf sizedCache size)

sizedCache :: Int -> Cache Row (Sum Int)
sizedCache size = initializeCache 2 (sizedSalesDomain size) sales

benchmarkInitializeCache2 :: Int -> Benchmark
benchmarkInitializeCache2 size =
  bench (show size) (nf sizedCache2 size)

sizedCache2 :: Int -> Cache2 Row (Sum Int)
sizedCache2 size = initializeCache2 (sizedSalesDomain size) sales

sizedSalesDomain :: Int -> [Row]
sizedSalesDomain size = do
  key <- [0 .. size]
  [Order key 2, LineItem key 20]

benchmarkUpdateCache :: Int -> Benchmark
benchmarkUpdateCache size =
  env (return (sizedCache size)) (\cache ->
    bench (show size) (whnf (updateCache rowUpdate) cache))

benchmarkUpdateCache2 :: Int -> Benchmark
benchmarkUpdateCache2 size =
  env (return (sizedCache2 size)) (\cache ->
    bench (show size) (whnf (updateCache2 rowUpdate) cache))

rowUpdate :: Row
rowUpdate = Order 0 2

