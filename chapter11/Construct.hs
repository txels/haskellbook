module Construct where 

data GuessWhat = Chickenbutt
    deriving (Eq, Show)

data Id a = MkId a
    deriving (Eq, Show)

data Product a b = Product a b
    deriving (Eq, Show)

data Sum a b = First a | Second b
    deriving (Eq, Show)

data RecordProduct a b = RecordProduct { pfirst :: a , psecond :: b }
    deriving (Eq, Show)


-- farm animals
type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age
    deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LovesMud
    deriving (Eq, Show)

data SheepInfo =
    SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal =
    Cow CowInfo
  | Pig PigInfo
  | Sheep SheepInfo
  deriving (Eq, Show)

data Animal' = Sum CowInfo (Sum PigInfo SheepInfo)


data Twitter =
    Twitter deriving (Eq, Show)
data AskFm =
    AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter


data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgrammingLanguage }
    deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill
    , Mac
    , Windows
    ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer os lang
                 | os <- allOperatingSystems
                 , lang <- allLanguages
                 ]
