module MakeTypes where


data PseudoBool = Fals | NiIdea | Cert
    deriving (Show, Eq, Ord, Enum)

anti :: PseudoBool -> PseudoBool
anti Cert = Fals
anti Fals = Cert
anti NiIdea = NiIdea


data Mood = Woot | Blah
instance Show Mood where
    show Woot = "Wooooot"
    show Blah = "Blaaaaah"

changeMood :: Mood -> Mood
changeMood Woot = Blah
changeMood Blah = Woot


-- parametric data constructors: Dog
type Name = String
data Pet = Cat | Dog Name deriving Show
