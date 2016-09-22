{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, RankNTypes #-}
module IncrementalQuery where


import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import Data.Hashable (hash)
import Control.Applicative (
  Alternative(empty, (<|>)))
import Control.Monad (
  MonadPlus(mzero, mplus), msum,
  guard, ap, (>=>))
import Data.Monoid (Product)


newtype Ring r a = Ring { runRing :: [(r, a)] }
  deriving (Show)

instance Functor (Ring r) where
  fmap f = Ring . fmap (fmap f) . runRing

instance (Monoid r) => Applicative (Ring r) where
  pure a = Ring (pure (pure a))
  fs <*> xs = Ring (do
    (rf, f) <- runRing fs
    (rx, x) <- runRing xs
    return (mappend rf rx, f x))

instance (Monoid r) => Alternative (Ring r) where
  empty = Ring empty
  rx1 <|> rx2 = Ring (runRing rx1 ++ runRing rx2)

instance (Monoid r) => Monad (Ring r) where
  return = pure
  ra >>= rab = Ring (do
    (ra, a) <- runRing ra
    (rb, b) <- runRing (rab a)
    return (mappend ra rb, b))

-- instance Bifunctor Ring where

type Occurence = Product Int

type Variable = String
type Database = Map TableName [Row]
type TableName = String
type ColumnName = String
type Row = Map ColumnName String

data Query f a where
  Pure :: a -> Query f a
  Bind :: f e -> (e -> Query f a) -> Query f a
  Zero :: Query f a -- not their Sum
  Plus :: Query f a -> Query f a -> Query f a

instance Monad (Query f) where
  return = Pure
  Pure a >>= k = k a
  Bind f k' >>= k = Bind f (k' >=> k)
  Zero >>= _ = Zero
  Plus q1 q2 >>= k= Plus (q1 >>= k) (q2 >>= k)

instance Applicative (Query f) where
  pure = Pure
  (<*>) = ap

instance Functor (Query f) where
  fmap f q = q >>= return . f

instance MonadPlus (Query f) where
  mzero = Zero
  mplus q1 q2 = Plus q1 q2 -- TODO more normalization

instance Alternative (Query f) where
  empty = mzero
  (<|>) = mplus

data Table a where
  Table :: TableName -> Table Variable

prettyQuery :: (Show a) => Query Table a -> String
prettyQuery (Pure a) =
  "return " ++ show a
prettyQuery (Bind (Table tablename) k) =
  "hi <- Table " ++ tablename ++ "\n" ++ prettyQuery (k "hi")
prettyQuery Zero =
  "mzero"
prettyQuery (Plus q1 q2) =
  "mplus (" ++ prettyQuery q1 ++ ") (" ++ prettyQuery q2 ++ ")"

exampleQuery :: Query Table (Variable, Variable)
exampleQuery = do
  x <- table "users"
  y <- table "items"
  return (x, y)

alternativeQuery :: Query Table Variable
alternativeQuery = do
  x <- (table "users" <|> table "items")
  return x

table :: TableName -> Query Table Variable
table tablename = Bind (Table tablename) Pure

delta :: TableName -> Row -> Query Table a -> Query Table a
delta t r = derivative (\(Table tablename) -> case t == tablename of
  False -> mzero
  True -> return (show r))

derivative :: (forall a . f a -> Query f a) -> Query f a -> Query f a
derivative _ (Pure _) =
  mzero
derivative deriveF (Bind fe k) = msum [
  deriveF fe >>= k,
  Bind fe (derivative deriveF . k),
  deriveF fe >>= derivative deriveF . k]
derivative _ Zero = mzero
derivative deriveF (Plus q1 q2) =
  mplus (derivative deriveF q1) (derivative deriveF q2)


-- | The degree of a query (like with polynomials)
degree :: Query Table a -> Integer
degree (Pure a) = 0
degree (Bind (Table _) k) = 1 + degree (k "oi")
degree (Zero) = 0
degree (Plus q1 q2) = max (degree q1) (degree q2)

-- degree (delta t r q) == max 0 (degree q - 1)

-- TODO what's with expressions ((==), ...)
-- TODO aggregates (what their comparison operators do)
-- TODO negation
-- FUTURE WORK recursive queries

-- Examples from "Ring of databases" paper


{-

example_4_1 :: Ring Occurence (String, String)
example_4_1 = r_A >>= (\(x, y) -> guard (y == "b1") >> return (x, y)) where
  r_A = Ring [(r1, ("a1", "b1")), (r2, ("a2", "b2"))]
  r1 = 1
  r2 = 1

{-
example_5_2 :: String -> Query ()
example_5_2 cid = do
  (c, n) <- customers
  (c', n') <- customers
  guard (n == n')
  guard (c == cid)
  return ()
-}

example :: Query Row
example = do
  u <- table "users"
  return u


delta :: TableName -> Row -> Query a -> Query a
delta t r (Pure a) = Mzero
delta t r (Fmap f q) = Fmap f (delta t r q)
delta t r (LiftA2 f q1 q2) = Mplus (Mplus (LiftA2 f (delta t r q1) q2) (LiftA2 f q1 (delta t r q2))) (LiftA2 f (delta t r q1) (delta t r q2))
delta t r (Join q) = Join (delta t r q) -- ??? Fmap (delta t r) (...)
delta t r (Mzero) = Mzero
delta t r (Mplus q1 q2) = Mplus (delta t r q1) (delta t r q2)
delta t r (Mnegate q) = Mnegate (delta t r q)
delta t r (Table t') = case t == t' of
  False -> Mzero
  True -> Pure r

-- q (insert t r d) = q d `Mplus` delta t r (q d) ???

-}
{-
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


createConference :: Text -> Event
createConference conferenceName = Insert "Conferences" (row ["ConferenceName" .= conferenceName])

createTalk :: ConferenceName -> TalkName -> Seats -> Event
createTalk = undefined

register :: ConferenceName -> AttendeeName -> Event
register = undefined

attend :: AttendeeName -> TalkName -> Event
attend = undefined


registered :: ConferenceName -> Query AttendeeName
registered = undefined

attendings :: ConferenceName -> Query (TalkName,AttendeeName)
attendings = undefined


-}
