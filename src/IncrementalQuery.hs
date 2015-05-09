{-# LANGUAGE GADTs #-}
module IncrementalQuery where

import Prelude hiding (filter)

import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map 
import Data.Maybe (maybeToList)
import Data.Hashable (hash)
import Control.Monad (guard)

data Timestamp = Integer

type TableName = String
type ColumnName = String
type Table = Set Row 
type Row = Map ColumnName String

type Database = Map TableName Table

data Event = Insert TableName Row

runEvent :: Event -> Database -> Database
runEvent (Insert tableName row) database = Map.adjust (Set.insert row) tableName database

data Query s a where
    Constant :: s -> a -> Query s a
    From :: s -> TableName -> Query s Row
    Empty :: s -> Query s a
    Project :: s -> (a -> b) -> Query s a -> Query s b
    Join :: s -> (a -> Int) -> (b -> Int) -> Query s a -> Query s b -> Query s (a,b)

queryState :: Query s a -> s
queryState (Empty s) = s
queryState (Constant s _) = s
queryState (From s _) = s
queryState (Project s _ _) = s
queryState (Join s _ _ _ _) = s

constant :: a -> Query () a
constant = Constant ()

from :: TableName -> Query () Row
from = From ()

filter :: (a -> Bool) -> Query s a -> Query s a
filter predicate = undefined

joinColumn :: ColumnName -> Query () Row -> Query () Row -> Query () Row
joinColumn columnName left right = Project () (uncurry Map.union)
    (Join () hashColumn hashColumn left right) where
        hashColumn = hash . (Map.lookup columnName)

runQuery :: Query s a -> Database -> Query [a] a
runQuery Empty _ = []
runQuery (Constant a) _ = [a]
runQuery (From _ tableName) database = maybe [] Set.toList (Map.lookup tableName database)
runQuery (Project _ function query) database = Prelude.map function (runQuery query database)
runQuery (Join _ leftHash rightHash left right) database = do
    leftRow <- runQuery left database
    rightRow <- runQuery right database
    guard (leftHash leftRow == rightHash rightRow)
    return (leftRow,rightRow)

unify :: Row -> Row -> Maybe Row
unify leftRow rightRow
    | and (Map.elems (Map.intersectionWith (==) leftRow rightRow)) = Just (Map.union leftRow rightRow)
    | otherwise = Nothing

data Delta a = Delta [a]
    deriving (Show,Eq,Ord)

runIncremental :: Query s a -> Event -> Delta a
runIncremental (Empty _) _ = Delta []
runIncremental (Constant _ row) _ = Delta [row]
runIncremental (From _ fromTableName) (Insert insertTableName row)
    | fromTableName == insertTableName = Delta [row]
    | otherwise = Delta []
runIncremental (Project _ function query) event = Delta (Prelude.map function rows) where
    Delta rows = runIncremental query event
runIncremental (Join _ leftHash rightHash left right) event = Delta (do
    let Delta addedLeft = runIncremental left event
        Delta addedRight = runIncremental right event
    return undefined)


main :: IO ()
main = do
--    print (runQuery exampleQuery exampleDatabase)
    putStrLn (unlines (Prelude.map show (queryState (runQuery exampleQuery (runEvent exampleEvent exampleDatabase)))))
--    print (runIncremental exampleQuery exampleEvent)


exampleQuery :: Query () Row
exampleQuery = joinColumn "LastName" (from "Person") (from "PhoneBook")

exampleDatabase :: Database
exampleDatabase = Map.fromList [
    ("Person",personTable),
    ("PhoneBook",phoneBookTable)]

exampleEvent :: Event
exampleEvent = Insert "Person" lukas where
    lukas = Map.fromList [
        ("FirstName","Lukas"),
        ("LastName","Haertel")]

personTable :: Table
personTable = Set.fromList [
    johannes,philipp] where
        johannes = Map.fromList [
            ("FirstName","Johannes"),
            ("LastName","Haertel")]
        philipp = Map.fromList [
            ("FirstName","Philipp"),
            ("LastName","Schuster")]

phoneBookTable :: Table
phoneBookTable = Set.fromList [
    lukasAddress,johannesAddress,philippAddress] where
        lukasAddress = Map.fromList [
            ("LastName","Haertel"),
            ("Address","Hohenzollernstr."),
            ("Number","01702835674")]
        johannesAddress = Map.fromList [
            ("LastName","Haertel"),
            ("Address","Hohenzollernstr."),
            ("Number","01519283343")]
        philippAddress = Map.fromList [
            ("LastName","Schuster"),
            ("Address","Moselweisserstr."),
            ("Number","015757237272")]
