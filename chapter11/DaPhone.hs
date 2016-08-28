module DaPhone where

type Digit = Char
type Presses = Int

-- |1     |2 ABC |3 DEF |
-- |4 GHI |5 JKL |6 MNO |
-- |7 PQRS|8 TUV |9 WXYZ|
-- |* ^   |0 +_  |# .,  |

-- TODO: p.478
data DaPhone = DaPhone
        

convo :: [String]
convo =
   ["Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps = undefined


cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead = undefined
