{-# LANGUAGE GADTs #-}
module IncrementalQuery where


import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map 
import Data.Maybe (fromMaybe)
import Data.Hashable (hash)
import Control.Monad (guard)


type Database = Map TableName Table
type TableName = String
type Table = [Row]
type ColumnName = String
type Row = Map ColumnName String

data Event = Insert TableName Row

runEvent :: Event -> Database -> Database
runEvent (Insert tableName row) database = Map.adjust (Set.insert row) tableName database

data Query a where
    Project :: (a -> b) -> Query a -> Query b
    Restrict :: (a -> Bool) -> Query a -> Query a
    Cross :: Query a -> Query b -> Query (a,b)
    Table :: TableName -> Query Row

runQuery :: Database -> Query a -> Query a
runQuery d (Project f q) = map f (runQuery d q)
runQuery d (Restrict p q) = filter p (runQuery d q)
runQuery d (Cross q r) = do
    a <- runQuery d q
    b <- runQuery d r
    return (a,b)
runQuery d (Table t) = fromMaybe [] (Map.lookup t d)

data Incremental a where
    IncrementalProject :: (a -> b) -> Incremental a -> Incremental b

incrementalize :: Query a -> Incremental a
incrementalize (Project f q) = IncrementalProject f (incrementalize q)

data Diff a = Diff [a] deriving (Eq,Ord,Show)

feed :: Event -> Incremental a -> Diff a
feed = undefined

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
