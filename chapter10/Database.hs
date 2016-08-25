module Database where
import Data.Time


data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    , DbNumber 12
    ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr justDate []
    where justDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
          justDate (DbDate x) y =  x : y
          justDate _ y = y

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _ = False

filterDbDate' = filter isDbDate

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr justInt []
    where justInt :: DatabaseItem -> [Integer] -> [Integer]
          justInt (DbNumber x) y =  x : y
          justInt _ y = y


mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sum numbers) / fromIntegral (length numbers)
    where numbers = filterDbNumber xs
