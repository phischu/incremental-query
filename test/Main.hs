module Main where

import IncrementalQuery (
  Query, database, guardEqual,
  Cache(Cache), initializeCache, updateCache,
  Clause(Clause),
  Equality(Equality), Expression(Variable, List, Pair))

import Test.Hspec (
  hspec, describe, it, shouldBe)

import qualified Data.Map as Map (
  fromList)

main :: IO ()
main = hspec (do
  describe "cross" (do

    let cross :: Query (Expression r r) (Expression r [(r, r)])
        cross = do
          x <- database
          y <- database
          return (List [Pair (x, y)])

        initialCache :: Cache Int [(Int, Int)]
        initialCache = initializeCache cross

        updatedCache :: Cache Int [(Int, Int)]
        updatedCache = Cache [(1,1)] derivativeClauses secondDerivativeClauses [1]

        updatedCache2 :: Cache Int [(Int, Int)]
        updatedCache2 = Cache [(1,1), (2,2), (2,1), (1,2)] derivativeClauses secondDerivativeClauses [1,2]

        derivativeClauses :: [Clause r [(r,r)]]
        derivativeClauses = [
          Clause [] (List [Pair (Variable "dx", Variable "dx")])]

        secondDerivativeClauses :: [Clause r [(r, r)]]
        secondDerivativeClauses = [
          Clause [] (List [Pair (Variable "dx", Variable "ddx")]),
          Clause [] (List [Pair (Variable "ddx", Variable "dx")])]

    it "does the first update" (
      updateCache 1 initialCache `shouldBe` updatedCache)
    it "does the second update" (
      updateCache 2 updatedCache `shouldBe` updatedCache2))

  describe "condition" (do

    let condition :: Query (Expression r r) (Expression r [(r,r)])
        condition = do
          x <- database
          y <- database
          guardEqual x y
          return (List [Pair (x, y)])

        initialCache = initializeCache condition

        updatedCache = Cache [(1,1)] derivativeClauses secondDerivativeClauses [1]

        updatedCache2 = Cache [(1,1), (2,2)] derivativeClauses secondDerivativeClauses [1,2]

        derivativeClauses = [
          Clause
            [Equality (Variable "dx") (Variable "dx")]
            (List [Pair (Variable "dx", Variable "dx")])]

        secondDerivativeClauses = [
          Clause
            [Equality (Variable "dx") (Variable "ddx")]
            (List [Pair (Variable "dx", Variable "ddx")]),
          Clause
            [Equality (Variable "ddx") (Variable "dx")]
            (List [Pair (Variable "ddx", Variable "dx")])]

    it "does the first update" (
      updateCache 1 initialCache `shouldBe` updatedCache)

    it "does the second update" (
      updateCache 2 updatedCache `shouldBe` updatedCache2)))


