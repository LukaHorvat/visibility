module Data.Geometry.Angle where

import Data.Fixed (mod')

newtype Angle = Angle Double deriving (Eq, Show, Read, Ord)

ccwFrom :: Angle -> Angle -> Bool
(Angle x) `ccwFrom` (Angle y) | x > y && x - y <  pi = True
                              | x < y && y - x >= pi = True
                              | otherwise            = False

ccwEqFrom :: Angle -> Angle -> Bool
a1 `ccwEqFrom` a2 = a1 == a2 || a1 `ccwFrom` a2

cwFrom :: Angle -> Angle -> Bool
cwFrom a b = not $ ccwEqFrom a b

cwEqFrom :: Angle -> Angle -> Bool
cwEqFrom a b = not $ ccwFrom a b

instance Num Angle where
    fromInteger = mkAngle . fromInteger
    (+) = error "Angles can't be added"
    (*) = error "Angles can't be multiplied"
    abs = error "Angles don't have absolute values"
    signum = error "Angles don't have signum"
    negate = error "Angle can't be negated"

instance Fractional Angle where
    fromRational = mkAngle . fromRational
    recip = error "Angles don't have a reciprocal"

sin' :: Angle -> Double
sin' (Angle x) = sin x

cos' :: Angle -> Double
cos' (Angle x) = cos x

data AngleSpan = AngleSpan !Angle !Angle deriving (Eq, Show, Read)

mkAngle :: Double -> Angle
mkAngle ang | ang >= 0 && ang < 2 * pi = Angle ang
            | otherwise                = Angle $! ang `mod'` (2 * pi)

angleInSpan :: Angle -> AngleSpan -> Bool
angleInSpan ang (AngleSpan first second) =
    (ang `ccwEqFrom` first && second `ccwEqFrom` ang) ||
    (ang `ccwEqFrom` first && first `ccwFrom` second) ||
    (ang `cwEqFrom` second && second `cwFrom` first)

compareRelative :: Angle -> Angle -> Angle -> Ordering
compareRelative orig a b
    | a == b                              = EQ
    | a `ccwFrom` orig && b `cwFrom` orig = LT
    | b `ccwFrom` orig && a `cwFrom` orig = GT
    | a `ccwFrom` b                       = GT
    | otherwise                           = LT
