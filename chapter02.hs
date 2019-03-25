-- haskellbook ch2

one = let x = 3; y = 1000 in x * 3 + y

one' = x*3 + y
    where x=3
          y=1000


two = let y = 10; x = 10 * 5 + y in x * 5

two' = x*5
    where y=10
          x = 10*5 + y


three = let x = 7; y = negate x; z = y * 10 in z / x + y

three' = z / x + y
    where x = 7
          y = (-x)
          z = y * 10

waxOn = x * 5
    where
        z = 7
        y = z + 8
        x = y ^ 2

triple = (*3)

waxOff x = triple x
