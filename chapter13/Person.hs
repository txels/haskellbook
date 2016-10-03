module Person where


type Name = String
type Age = Integer

data Person = Person Name Age
    deriving (Eq, Show)

data PersonInvalid
    = NameEmpty
    | AgeTooLow
    | PersonInvalidUnkown String
    deriving (Eq, Show)

mkPerson :: String -> Integer -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age >= 0 = Right (Person name age)
    | name == "" = Left NameEmpty
    | age < 0 = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnkown $
        "Name was: " ++ show name ++ " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStr "Name: "
    name <- getLine
    putStr "Age:  "
    ageString <- getLine
    let age = (read ageString)::Integer;
        person = mkPerson name age
        in
        case person of
            (Right person) -> putStrLn $ "Yay! " ++ show person
            (Left noPerson) -> putStrLn $ "Nopes! " ++ show noPerson

