{-# LANGUAGE GADTs, TypeSynonymInstances, FlexibleInstances, RankNTypes, DeriveGeneric, StandaloneDeriving #-}
module IncrementalQuery where


import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust, mapMaybe)
import Data.List (intercalate)
import Data.Hashable (hash)
import Control.Applicative (
  Alternative(empty, (<|>)), liftA2)
import Control.Monad (
  MonadPlus(mzero, mplus), msum,
  guard, ap, (>=>))
import Data.Monoid (
  Sum(Sum))
import Control.DeepSeq (
  NFData)
import GHC.Generics (
  Generic)


cross :: Query r (r, r)
cross = do
  x <- database
  y <- database
  return (x, y)


condition :: Query r r
condition = do
  x <- database
  y <- database
  guardEqual x y
  return x

{-
sales :: Query Row (Sum Int)
sales = do
  Order orderKey orderExchange <- database
  LineItem lineItemKey lineItemPrice <- database
  guard (orderKey == lineItemKey)
  return (Sum (lineItemPrice * orderExchange))
-}
salesDomain :: [Row]
salesDomain = [
  Order 0 1,
  Order 1 2,
  LineItem 0 10,
  LineItem 0 20,
  LineItem 1 30]


data Row = Order Key Exchange | LineItem Key Price
  deriving (Show, Eq, Ord, Generic)

instance NFData Row

type Key = Int
type Exchange = Int
type Price = Int



data FreerMonadPlus f a where
  Pure :: a -> FreerMonadPlus f a
  Bind :: f e -> (e -> FreerMonadPlus f a) -> FreerMonadPlus f a
  Zero :: FreerMonadPlus f a
  Plus :: FreerMonadPlus f a -> FreerMonadPlus f a -> FreerMonadPlus f a

instance Monad (FreerMonadPlus f) where
  return = Pure
  Pure a     >>= k = k a
  Bind f k'  >>= k = Bind f (k' >=> k)
  Zero       >>= _ = Zero
  Plus q1 q2 >>= k = Plus (q1 >>= k) (q2 >>= k)
  fail _ = Zero

instance Applicative (FreerMonadPlus f) where
  pure = Pure
  (<*>) = ap

instance Functor (FreerMonadPlus f) where
  fmap f q = q >>= return . f

instance MonadPlus (FreerMonadPlus f) where
  mzero = Zero
  mplus q1 q2 = Plus q1 q2 -- TODO more normalization

instance Alternative (FreerMonadPlus f) where
  empty = mzero
  (<|>) = mplus


type Query r a = FreerMonadPlus (Database r) a

data Database r a where
  Database :: Database r r
  GuardEqual :: r -> r -> Database r ()

database :: Query r r
database = Bind Database Pure

guardEqual :: r -> r -> Query r ()
guardEqual x y = Bind (GuardEqual x y) Pure

runQuery :: [r] -> Query r a -> [a]
runQuery rows = foldQuery rows . fmap (:[])

foldQuery :: (Monoid a) => [r] -> Query r a -> a
foldQuery rows (Pure a) = a
foldQuery rows (Bind Database k) = mconcat (map (foldQuery rows . k) rows)
foldQuery _    (Zero) = mempty
foldQuery rows (Plus q1 q2) = mappend (foldQuery rows q1) (foldQuery rows q2)


derivative :: r -> Query r a -> Query r a
derivative _ (Pure _) =
  mzero
derivative row (Bind Database k) = msum [
  pure row >>= k,
  database >>= derivative row . k,
  pure row >>= derivative row . k]
derivative row (Bind (GuardEqual e1 e2) k) =
  guardEqual e1 e2 >>= derivative row . k
derivative _ (Zero) =
  mzero
derivative d (Plus q1 q2) = msum [
  derivative d q1,
  derivative d q2]

runIncremental :: (Monoid a) => [r] -> Query r a -> r -> a
runIncremental rows query delta = foldQuery rows (derivative delta query)


type Variable = String

variables :: [Variable]
variables = ["x", "y", "z"]

data Expression v a where
  Variable :: Variable -> Expression v v
  Pair :: (Expression v b, Expression v c) -> Expression v (b, c)
  List :: [Expression v b] -> Expression v [b]

deriving instance Show (Expression v a)
deriving instance Eq (Expression v a)

printQuery :: (Show a) => Query (Expression Variable Variable) a -> IO ()
printQuery = putStrLn . showQuery variables . simplify

showQuery :: (Show a) => [Variable] -> Query (Expression Variable Variable) a -> String
showQuery _ (Pure a) =
  "pure " ++ (show a)
showQuery (v : vs) (Bind Database k) =
  "database >>= (\\" ++ v ++ " -> " ++ showQuery vs (k (Variable v))++ ")"
showQuery _ (Zero) =
  "mzero"
showQuery vs (Plus q1 q2) =
  "mplus (" ++ showQuery vs q1 ++ ") (" ++ showQuery vs q2 ++ ")" where


simplify :: Query v a -> Query v a
simplify (Pure a) =
  Pure a
simplify (Bind c k) =
  case simplify (k undefined) of
    Zero -> Zero
    _ -> Bind c (simplify . k)
simplify Zero =
  Zero
simplify (Plus q1 q2) =
  case (simplify q1, simplify q2) of
    (Zero, Zero) -> Zero
    (Zero, _) -> simplify q2
    (_, Zero) -> simplify q1
    _ -> Plus (simplify q1) (simplify q2)


-- | The current value, the first derivative, the second derivative and
-- an Index. The index is used to look up rows for which the second derivative
-- is not zero.
data Cache r a = Cache a [Clause r a] [Clause r a] [Index r]
  deriving (Show, Eq)

-- XXX Initialize result to query on empty database
initializeCache :: (Monoid a) => Query (Expression r r) (Expression r a) -> Cache r a
initializeCache query =
  Cache mempty derivativeClauses secondDerivativeClauses indexes where
    derivativeClauses =
      queryClauses (derivative (Variable "dx") query)
    secondDerivativeClauses =
      queryClauses (derivative (Variable "ddx") (derivative (Variable "dx") query))
    indexes =
      replicate (length secondDerivativeClauses) emptyIndex

updateCache :: (Ord r, Monoid a) => r -> Cache r a -> Cache r a
updateCache row (Cache result derivativeClauses secondDerivativeClauses indexes) =
  Cache result' derivativeClauses secondDerivativeClauses indexes' where
    result' = mappend result (mappend derivativeDelta secondDerivativeDelta)
    derivativeDelta =
      runClauses [row] derivativeClauses
    secondDerivativeDelta =
      mconcat (do
        (secondDerivativeClause, index) <- zip secondDerivativeClauses indexes
        return (mconcat (do
          row2 <- lookupIndex secondDerivativeClause row index
          return (runClause [row, row2] secondDerivativeClause))))
    indexes' =
      zipWith (\clause -> insertIndex clause row) secondDerivativeClauses indexes

-- | An additive clause.
data Clause r a where
  Clause :: [Equality r a] -> Expression r a -> Clause r a
    deriving (Show, Eq)

-- TODO Existentially hide parameter a
data Equality r a = Equality (Expression r r) (Expression r r)
  deriving (Show, Eq)

runClauses :: (Eq r, Monoid a) => [r] -> [Clause r a] -> a
runClauses deltas clauses = mconcat (map (runClause deltas) clauses)

runClause :: (Eq r, Monoid a) => [r] -> Clause r a -> a
runClause deltas (Clause equalities expression) =
  case evaluateEqualities deltas equalities of
    False -> mempty
    True -> evaluateExpression deltas expression

evaluateEqualities :: (Eq r) => [r] -> [Equality r a] -> Bool
evaluateEqualities deltas equalities =
  all (evaluateEquality deltas) equalities

evaluateEquality :: (Eq r) => [r] -> Equality r a -> Bool
evaluateEquality deltas (Equality expression1 expression2) =
  evaluateExpression deltas expression1 == evaluateExpression deltas expression2

evaluateExpression :: [r] -> Expression r a -> a
evaluateExpression (dx : _) (Variable "dx") =
  dx
evaluateExpression (_ : ddx : _) (Variable "ddx") =
  ddx
evaluateExpression ds (Pair (e1, e2)) =
  (evaluateExpression ds e1, evaluateExpression ds e2)
evaluateExpression ds (List es) =
  map (evaluateExpression ds) es

queryClauses :: Query (Expression r r) (Expression r a) -> [Clause r a]
queryClauses (Pure e) =
  [Clause [] e]
queryClauses (Bind Database k) =
  []
queryClauses (Bind (GuardEqual e1 e2) k) = do
  Clause equalities result <- queryClauses (k ())
  return (Clause (Equality e1 e2 : equalities) result)
queryClauses Zero =
  []
queryClauses (Plus q1 q2) =
  queryClauses q1 ++ queryClauses q2


type Index r = Map [r] [r]

emptyIndex :: Index r
emptyIndex = Map.empty

lookupIndex :: (Ord r) => Clause r a -> r -> Index r -> [r]
lookupIndex (Clause equalities _) row index =
  fromMaybe [] (Map.lookup (equalitiesKey row equalities) index)

insertIndex :: (Ord r) => Clause r a -> r -> Index r -> Index r
insertIndex (Clause equalities _) row2 index =
  Map.insertWith (++) (equalitiesKey row2 equalities) [row2] index

equalitiesKey :: r -> [Equality r a] -> [r]
equalitiesKey row = mapMaybe (equalityKey row)

-- TODO different function for lookup and insertion
equalityKey :: r -> Equality r a -> Maybe r
equalityKey row (Equality (Variable "dx") (Variable "dx")) =
  Nothing
equalityKey row (Equality (Variable "dx") (Variable "ddx")) =
  Just row
equalityKey row (Equality (Variable "ddx") (Variable "dx")) =
  Just row
equalityKey row (Equality (Variable "ddx") (Variable "ddx")) =
  Nothing



{-
-- | In our limited language we have only one kind of equality constraint
type Equality = ()

applyEqualitiesLeft :: [Equality] -> r -> [r]
applyEqualitiesLeft equalities r =
  replicate (length equalities) r

applyEqualitiesRight :: [Equality] -> r -> [r]
applyEqualitiesRight equalities r =
  replicate (length equalities) r



lookupIndex :: (Ord r, Monoid a) => r -> Summand r a -> [r]
lookupIndex delta (Summand equalities index) =
  fromMaybe [] (Map.lookup (applyEqualitiesLeft equalities delta) index)

insertIndex :: (Ord r, Monoid a) => r -> Summand r a -> Summand r a
insertIndex delta (Summand equalities index) =
  Summand equalities (Map.insertWith (++) (applyEqualitiesRight equalities delta) [delta] index)
-}
{-
main :: IO ()
main = do
  printQuery cross
  printQuery (derivative "d" cross)
  printQuery (derivative "dd" (derivative "d" cross))
  putStrLn ""
  print (initializeCache 2 [1] cross)
  print (updateCache 1 (initializeCache 2 [1] cross))
  print (initializeCache 2 [1,2] cross)
  print (updateCache 2 (updateCache 1 (initializeCache 2 [1,2] cross)))
  putStrLn ""
  print (initializeCache 1 [1,2] condition)
  print (updateCache 2 (initializeCache 1 [1,2] condition))
  print (updateCache 1 (initializeCache 1 [1,2] condition))
  putStrLn ""
  print (initializeCache 2 salesDomain sales)
  print (updateCache (LineItem 0 20) (updateCache (LineItem 0 10) (updateCache (Order 0 1) (initializeCache 2 salesDomain sales))))

data Cache v a = Cache a (Map v (Cache v a))
  deriving (Show, Generic)

instance (NFData r, NFData a) => NFData (Cache r a)

updateCache :: (Ord v, Monoid a) => v -> Cache v a -> Cache v a
updateCache delta (Cache results caches) = Cache results' caches' where
  results' = case Map.lookup delta caches of
    Just (Cache deltaResults _) -> results `mappend` deltaResults
    Nothing -> results
  caches' = Map.map (updateCache delta) caches

initializeCache :: (Ord v, Monoid a) => Int -> [v] -> Query v a -> Cache v a
initializeCache depth domain query = Cache mempty (initializeCaches depth domain query)

initializeCaches :: (Ord v, Monoid a) => Int -> [v] -> Query v a -> Map v (Cache v a)
initializeCaches 0 _ _ = Map.empty
initializeCaches depth domain query = Map.fromList (do
  delta <- domain
  let derivativeQuery = derivative delta query
      deltaResults = foldQuery [] derivativeQuery
      caches = initializeCaches (depth - 1) domain derivativeQuery
      cache = Cache deltaResults caches
  return (delta, cache))


data Cache2 r a = Cache2 !a !(Map r a) !(Map r (Map r a))
  deriving (Show, Generic)

instance (NFData r, NFData a) => NFData (Cache2 r a)

emptyCache2 :: (Monoid a) => Cache2 r a
emptyCache2 = Cache2 mempty Map.empty Map.empty

updateCache2 :: (Ord r, Monoid a) => r -> Cache2 r a -> Cache2 r a
updateCache2 delta (Cache2 result derivativeCache secondDerivativeCaches) =
  Cache2 result' derivativeCache' secondDerivativeCaches where
    result' = case Map.lookup delta derivativeCache of
      Just derivative -> result `mappend` derivative
      Nothing -> error "Delta not in derivative cache"
    derivativeCache' = case Map.lookup delta secondDerivativeCaches of
      Just secondDerivatives -> Map.unionWith mappend secondDerivatives derivativeCache
      Nothing -> error "Delta not in secondDerivativeCache"

initializeCache2 :: (Ord r, Monoid a) => [r] -> Query r a -> Cache2 r a
initializeCache2 domain query =
  Cache2 mempty derivativeCaches secondDerivativeCaches where
    derivativeCaches = Map.fromList (do
      delta <- domain
      let derivativeQuery = derivative delta query
          derivativeResults = foldQuery [] derivativeQuery
      return (delta, derivativeResults))
    secondDerivativeCaches = Map.fromList (do
      secondDelta <- domain
      let secondDerivatives = Map.fromList (do
            delta <- domain
            let secondDerivativeQuery =
                  derivative secondDelta (derivative delta query)
                secondDerivativeResults =
                  foldQuery [] secondDerivativeQuery
            guard (not (null (foldQuery [] (fmap (:[]) secondDerivativeQuery))))
            return (delta, secondDerivativeResults))
      return (secondDelta, secondDerivatives))

insertCache2 :: (Ord r, Monoid a) => [r] -> r -> Query r a -> Cache2 r a -> Cache2 r a
insertCache2 rows delta query (Cache2 result derivativeCache secondDerivativesCache) =
  Cache2 result' derivativeCache' secondDerivativesCache' where

    result' =
      mappend result deltaResult

    derivativeCache' =
      Map.unionsWith mappend [
        derivativeCache, deltaDerivative, deltaDerivatives]

    secondDerivativesCache' =
      Map.unionsWith (Map.unionWith mappend) [
        secondDerivativesCache, deltaSecondDerivative, deltaSecondDerivatives]

    deltaResult =
      foldQuery rows (derivative delta query)

    deltaDerivative =
      Map.singleton delta deltaResult

    deltaDerivatives = Map.fromListWith mappend (do
      row <- rows ++ [delta]
      let secondDerivativeQuery =
            derivative delta (derivative row query)
          secondDerivativeResult =
            foldQuery rows secondDerivativeQuery
      return (row, secondDerivativeResult))

    deltaSecondDerivative = Map.singleton delta (Map.fromListWith mappend (do
      row <- rows
      let secondDerivativeQuery =
            derivative delta (derivative row query)
          secondDerivativeResult =
            foldQuery rows secondDerivativeQuery
      return (row, secondDerivativeResult)))

    deltaSecondDerivatives = (Map.fromListWith (Map.unionWith mappend) (do
      row <- rows ++ [delta]
      let secondDerivativeQuery =
            derivative row (derivative delta query)
          secondDerivativeResult =
            foldQuery rows secondDerivativeQuery
      return (row, Map.singleton delta secondDerivativeResult)))
-}
{-
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

type Database = Map TableName [Row]
type TableName = String

data Table a where
  Table :: TableName -> Table Expression

data Expression =
  Variable String |
  Constant Rational |
  Field Expression String |
  Equal Expression Expression |
  Smaller Expression Expression |
  Larger Expression Expression


multiplicity :: a -> Rational -> Query f a
multiplicity = undefined

factor :: Expression -> Query f ()
factor = undefined

guard' :: Expression -> Query f ()
guard' = undefined

runSubquery :: Query f () -> Query f Rational
runSubquery = undefined

prettyQuery :: (Show a) => Query Table a -> String
prettyQuery (Pure a) =
  "return " ++ show a
prettyQuery (Bind (Table tablename) k) =
  "hi <- Table " ++ tablename ++ "\n" ++ prettyQuery (k (Variable "hi"))
prettyQuery Zero =
  "mzero"
prettyQuery (Plus q1 q2) =
  "mplus (" ++ prettyQuery q1 ++ ") (" ++ prettyQuery q2 ++ ")"

exampleQuery :: Query Table (Expression, Expression)
exampleQuery = do
  x <- table "users"
  y <- table "items"
  return (x, y)

alternativeQuery :: Query Table Expression
alternativeQuery = do
  x <- (table "users" <|> table "items")
  return x

table :: TableName -> Query Table Expression
table tablename = Bind (Table tablename) Pure

delta :: TableName -> Row -> Query Table a -> Query Table a
delta t r = derivative (\(Table tablename) -> case t == tablename of
  False -> mzero
  True -> return (Variable (show r)))

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
degree (Bind (Table _) k) = 1 + degree (k (Variable "oi"))
degree (Zero) = 0
degree (Plus q1 q2) = max (degree q1) (degree q2)

-- degree (delta t r q) == max 0 (degree q - 1)

-- TODO aggregates (what their comparison operators do) (== subqueries?)
-- TODO negation
-- TODO range joins?
-- TODO recursive queries
-- TODO first class indexes


-- Examples from "DBToaster" extended report.

-- | Count the number of tuples in the product of 'R' and 'S'.
example_1 :: Query Table ()
example_1  = liftA2 (const (const ())) (table "R") (table "S")

-- | Total sales across all orders weighted by exchange rate.
example_2 :: Query Table ()
example_2 = do
  o <- table "Orders"
  li <- table "LineItems"
  guard' (Equal (Field o "ORDK") (Field li "ORDK"))
  multiplicity () (undefined (Field li "PRICE") (Field o "XCH"))

data Order = Order {
  orderORDK :: Int,
  orderXCH :: Rational
  }

data LineItem = LineItem {
  lineItemORDK :: Int,
  lineItemPRICE :: Rational
  }

example_3 :: Query Table (Expression, Expression)
example_3 = do
  r <- table "R"
  let x = Field r "A"
      y = Field r "B"
  guard' (Smaller x y)
  return (x, y)


example_4 :: Query Table Expression
example_4 = do
  r <- table "R"
  let x = Field r "A"
      y = Field r "B"
  factor (Constant 2)
  factor x
  return y

example_5 :: Query Table Expression
example_5 = do
  r <- table "R"
  n <- runSubquery (do
     s <- table "S"
     guard' (Larger (Field r "A") (Field s "C"))
     factor (Field s "D")
     return ())
  guard' (Smaller (Field r "B") (Constant n))
  -- guard' (Smaller (Field r "B") (SubQuery q))
  return r

example_6 :: Query Table ()
example_6 = do
  r <- table "R"
  s <- table "S"
  guard' (Equal (Field r "B") (Field s "C"))
  factor (Field r "A")
  factor (Field s "D")
  return ()

-- | Delta of assignment.
example_7 = error "uses delta inside"

-- | Taking deltas adds input variables.
example_8 = error "delta example_5"

-- | Second order delta for example 6
example_9 = error "second order delta"

-- | We don't have to materialize all subqueries.
example_11 = error "delta query of example 7"

-- | Query simplification
example_12 = error "simplification not implemented"

-- | Reducing assignments
example_13 = error "assignment is not implemented"

-- TODO convincing example that DBToaster approach is better than usual incrementalization
-- Perhaps because it subsumes all optimizations from `Reify your collection API ...`
-}
-- Examples from "Incremental Collection Programs" paper

{-

-- Example 1

related = do
  m <- movies
  return (name m, relB m)

relB m = do
  m2 <- movies
  guard (isRelated m m2)
  return m2.name

isRelated m m2 = name m /= name m2 && (genre m == genre m2 || director m == director m2)


-- Example 2

filter p = do
  x <- database
  guard (p x)
  return x


-- Example 3

delta (filter p) r dr = do
  x <- dr
  guard (p x)
  return x


-- Example 4

h r = liftA2 (,) (flatten r) (flatten r)

delta h r dr =
  mplus (cross (flatten r) (flatten dr)) (cross (flatten dr) (mplus (flatten r) (flatten dr)))

delta (delta h) r dr ddr =
  mplus (cross (flatten ddr) (flatten dr)) (cross (flatten dr) (flatten ddr))

-- initialize
h0 = materialize (h r)
h1 dr = materialize (delta h r dr)

-- update
h0 += mplus h0 (h1 u)
h1 dr += mplus (h1 dr) (delta (delta h) undefined dr u)


-- Example 5

cost (related m) = BagCost (size m) (TupleCost 1 (BagCost (size m) 1))
running_time_upper_bound (related m) = size m * (1 + size m)

-}
{-

-- Examples from "Ring of Databases" paper

example_4_1 :: Ring Occurence (String, String)
example_4_1 = r_A >>= (\(x, y) -> guard (y == "b1") >> return (x, y)) where
  r_A = Ring [(r1, ("a1", "b1")), (r2, ("a2", "b2"))]
  r1 = 1
  r2 = 1

example_5_2 :: String -> Query ()
example_5_2 cid = do
  (c, n) <- customers
  (c', n') <- customers
  guard (n == n')
  guard (c == cid)
  return ()

example :: Query Row
example = do
  u <- table "users"
  return u

-}

{-
Looks like we need a real polynomial normal form e.g. transform a program like

query = do
  x <- table "R"
  if (field x "A" == "Value")
    then return x
    else empty

into

query = do
  x <- tableWithFieldEqual "R" "A" "Value"
  return x
    `mplus` do
  x <- tableWithFieldUnequal "R" "A" "Value"
  empty

TODO how to apply this transformation during construction? Does the Decidable class play a role?

-}



{-
-- Other examples

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
