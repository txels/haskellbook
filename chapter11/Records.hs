module Records where

-- not a record type:

data PersonA = MkPerson String Int
    deriving (Eq, Show)

txel = MkPerson "txels" 49
mart = MkPerson "Marta" 44

-- as a record type:

data PersonR =
    Person { name :: String
           , age :: Int }
    deriving (Eq, Show)

txels = Person "txels" 49
marta = Person "Marta" 44

nameMarta = name marta
combinedAge = sum (map age [marta, txels])

