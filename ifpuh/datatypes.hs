#!/usr/bin/env hugs

leapyear :: Integer -> Bool
leapyear y = (y `mod` 4 == 0) && (y `mod` 100 /= 0 || y `mod` 400 == 0)

isDigit, isLower, isUpper :: Char -> Bool

isDigit c = ('0' <= c) && (c <= '9')
isLower c = ('a' <= c) && (c <= 'z')
isUpper c = ('A' <= c) && (c <= 'Z')

capitalise :: Char -> Char
capitalise c = if isLower c then toEnum(offset + (fromEnum c)) else c
    where offset = fromEnum('A') - fromEnum('a')


-- Enumerations
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat

instance Enum Day where
    toEnum 0 = Sun
    toEnum 1 = Mon
    toEnum 2 = Tue
    toEnum 3 = Wed
    toEnum 4 = Thu
    toEnum 5 = Fri
    toEnum 6 = Sat
    fromEnum Sun = 0
    fromEnum Mon = 1
    fromEnum Tue = 2
    fromEnum Wed = 3
    fromEnum Thu = 4
    fromEnum Fri = 5
    fromEnum Sat = 6
{-
instance Enum Char where
    toEnum = ord
    fromEnum = chr
-}


instance Eq Day where
    x == y = (fromEnum(x) == fromEnum(y))

instance Ord Day where
    compare x y = if ox == oy then EQ else
                    if ox < oy then LT else GT
                    where ox = fromEnum(x); oy = fromEnum(y)

workday :: Day -> Bool
workday d = (Mon <= d) && (d <= Fri)

restday :: Day -> Bool
restday d = not (workday d)

dayAfter :: Day -> Day
dayAfter d = toEnum((fromEnum(d) + 1) `mod` 7)


data DevDay = DevSun | DevMon | DevTue | DevWed | DevThu | DevFri | DevSat
    deriving(Eq, Ord, Enum)


-- Tuple





-- Type synonyms
type Position = (Integer, Integer)
type Angle = Float
type Distance = Float

-- move :: Distance -> Angle -> Position -> Position
-- move d a (x, y) = (ceiling(x + d * cos a), ceiling(y + d * sin a))




