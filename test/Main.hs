module Main where

import IncrementalQuery (
  Query, database,
  Cache(Cache), initializeCache, updateCache,
  Clause(Clause),
  Expression(Variable, List, Pair))

import Test.Hspec (
  hspec, describe, it, shouldBe)

import qualified Data.Map as Map (
  fromList)

main :: IO ()
main = hspec (do
  describe "updateCache" (do
    it "does the first update" (
      updateCache 1 initialCache `shouldBe` updatedCache)
    it "does the second update" (
      updateCache 2 updatedCache `shouldBe` updatedCache2)))


cross :: Query (Expression r r) (Expression r [(r, r)])
cross = do
  x <- database
  y <- database
  return (List [Pair (x, y)])


initialCache :: Cache Int [(Int, Int)]
initialCache = initializeCache cross

updatedCache :: Cache Int [(Int, Int)]
updatedCache = Cache [(1,1)] crossDerivativeClauses crossSecondDerivativeClauses [1]

updatedCache2 :: Cache Int [(Int, Int)]
updatedCache2 = Cache [(1,1), (2,2), (2,1), (1,2)] crossDerivativeClauses crossSecondDerivativeClauses [1,2]

crossDerivativeClauses :: [Clause r [(r,r)]]
crossDerivativeClauses = [
  Clause [] (List [Pair (Variable "dx", Variable "dx")])]

crossSecondDerivativeClauses :: [Clause r [(r, r)]]
crossSecondDerivativeClauses = [
  Clause [] (List [Pair (Variable "dx", Variable "ddx")]),
  Clause [] (List [Pair (Variable "ddx", Variable "dx")])]

