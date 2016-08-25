module GlobalLocal where

topLevelFunc :: Integer -> Integer
topLevelFunc x = x + woot + topLevelVal
    where woot :: Integer
          woot = 10

topLevelVal :: Integer
topLevelVal = 5
