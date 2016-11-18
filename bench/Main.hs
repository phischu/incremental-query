module Main where

import IncrementalQuery (
  initializeCache, updateCache, Cache, Row(Order, LineItem), salesDomain, sales)
import Data.Monoid (
  Sum)

import Criterion.Main (
  defaultMain, bgroup, bench, nf, Benchmark, env)

main :: IO ()
main = defaultMain [
  bgroup "initializeCache" (map benchmarkInitializeCache [10, 20, 30, 40]),
  bgroup "updateCache" (map benchmarkUpdateCache [10, 20, 30, 40])]

benchmarkInitializeCache :: Int -> Benchmark
benchmarkInitializeCache size =
  bench (show size) (nf sizedCache size)

sizedCache :: Int -> Cache Row (Sum Int)
sizedCache size = initializeCache 2 (sizedSalesDomain size) sales

sizedSalesDomain :: Int -> [Row]
sizedSalesDomain size = do
  key <- [0 .. size]
  [Order key 2, LineItem key 20]

benchmarkUpdateCache :: Int -> Benchmark
benchmarkUpdateCache size =
  env (return (sizedCache size)) (\cache ->
    bench (show size) (nf (updateCache rowUpdate) cache))

rowUpdate :: Row
rowUpdate = Order 0 2

