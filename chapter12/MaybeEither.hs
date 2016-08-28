module MaybeEither where

type Name = String
type Age = Integer

data Person = Person Name Age
    deriving (Eq, Show)

mkPerson :: String -> Integer -> Maybe Person
mkPerson name age
    | name /= "" && age >= 0 = Just (Person name age)
    | otherwise = Nothing

-- improve with type of error:

data PersonInvalid
    = NameEmpty
    | AgeTooLow
    deriving (Eq, Show)

mkPerson' :: String -> Integer -> Either PersonInvalid Person
mkPerson' name age
    | name == "" = Left NameEmpty
    | age < 0 = Left AgeTooLow
    | otherwise = Right (Person name age)

-- improve with list of errors:

type ValidatePerson a = Either [PersonInvalid] a

ageOk :: Age -> ValidatePerson Age
ageOk age = case age >= 0 of
    True -> Right age
    False -> Left [AgeTooLow]

nameOk :: Name -> ValidatePerson Name
nameOk name = case name /= "" of
    True -> Right name
    False -> Left [NameEmpty]

mkPerson'' :: String -> Integer -> ValidatePerson Person
mkPerson'' name age = mkPerson2 (nameOk name) (ageOk age)

mkPerson2 :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person
mkPerson2 (Right name) (Right age) = Right (Person name age)
mkPerson2 (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson2 (Left badName) _ = Left (badName)
mkPerson2 _ (Left badAge) = Left (badAge)



mkPersonLift :: Name
             -> Age
             -> ValidatePerson Person
mkPersonLift name age = undefined
    -- TODO: whenever liftA2 is introduced
    -- import ... (liftA2)
    -- liftA2 Person (nameOk name) (ageOk age)
