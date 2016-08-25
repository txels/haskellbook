module HigherOrder where


flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x


data Employee = Coder
              | Manager
              | Veep
              | CEO
    deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " bosses " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering)
             -> Employee
             -> Employee
             -> IO ()
employeeRank ord e e' = case ord e e' of
    GT -> reportBoss e e'
    LT -> flip reportBoss e e'
    EQ -> putStrLn "No bossin', babe"

codersRule :: Employee -> Employee -> Ordering
codersRule Coder Coder = EQ
codersRule Coder _     = GT
codersRule _ Coder     = LT
codersRule e e'        = compare e e'


dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = (flip dodgy) 2

