{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
module SafeNum where

import Numeric.Natural
import GHC.Num.Integer
import GHC.Int
import GHC.Real
import Data.Ratio
import Data.Word

----------
-- SafeNum
----------

class Num a => SafeNum a where
  {- | Convert an 'Integer' into an 'a', safely.

    'fromIntegerMaybe' should satisfy the following laws:

        * 'fromIntegerMaybe' is always total, i.e. @fromIntegerMaybe x = _|_ ==> x = _|_@
        * 'fromIntegerMaybe' agrees with 'fromInteger' on its domain of definition,
          i.e. @fromIntegerMaybe i == Just j ==> fromInteger i = j@
        * If 'a' is an instance of 'Integral', then 'fromIntegerMaybe' is an inverse to 'toInteger' on its domain of definition,
          i.e. @fromIntegerMaybe (toInteger i) == Just i@ and @fromIntegerMaybe i == Just j ==> toInteger j = i@
  -}
  fromIntegerMaybe :: Integer -> Maybe a

fromIntegralMaybe :: (Integral a, SafeNum b) => a -> Maybe b
fromIntegralMaybe = fromIntegerMaybe . toInteger

-- | Implement 'fromIntegerMaybe' by checking whether the argument is inside the bounds of the type when projected into
-- 'Integer', and returning 'Nothing' otherwise. Not universally safe, as nothing guarantees that 'fromInteger' will
-- behave well in this range.
fromIntegerBounded :: forall a . (Ord a, Bounded a, Integral a) => Integer -> Maybe a
fromIntegerBounded n = if n < toInteger (minBound :: a) || n > toInteger (maxBound :: a) then Nothing else Just (fromInteger n)

instance SafeNum Integer where
  fromIntegerMaybe i = Just i

instance SafeNum Natural where
  fromIntegerMaybe i | i < 0 = Nothing
  fromIntegerMaybe i | otherwise = Just $ fromInteger i

instance SafeNum Int where
  fromIntegerMaybe i = case i of
    IS i -> Just (I# i)
    _ -> Nothing

-- Probably all of these could have hand-coded fast instances

instance SafeNum Int8 where
  fromIntegerMaybe = fromIntegerBounded

instance SafeNum Int16 where
  fromIntegerMaybe = fromIntegerBounded

instance SafeNum Int32 where
  fromIntegerMaybe = fromIntegerBounded

instance SafeNum Int64 where
  fromIntegerMaybe = fromIntegerBounded

instance SafeNum Word where
  fromIntegerMaybe = fromIntegerBounded

instance SafeNum Word8 where
  fromIntegerMaybe = fromIntegerBounded

instance SafeNum Word16 where
  fromIntegerMaybe = fromIntegerBounded

instance SafeNum Word32 where
  fromIntegerMaybe = fromIntegerBounded

instance SafeNum Word64 where
  fromIntegerMaybe = fromIntegerBounded

instance SafeNum Double where
  fromIntegerMaybe = Just . fromInteger

instance (Integral a, SafeNum a) => SafeNum (Ratio a) where
  fromIntegerMaybe i = (:%) <$> fromIntegerMaybe i <*> fromIntegerMaybe 1

-----------------
-- SafeFractional
-----------------

class Fractional a => SafeFractional a where
  {- | Convert a 'Rational' into an 'a', safely.

    'fromRationalMaybe' should satisfy the following laws:

        * 'fromRationalMaybe' is always total, i.e. @fromRationalMaybe x = _|_ ==> x = _|_@
        * 'fromRationalMaybe' agrees with 'fromRational' on its domain of definition,
          i.e. @fromRationalMaybe i == Just j ==> fromRational i = j@
        * If 'a' is an instance of 'Real', then 'fromRationalMaybe' is an inverse to 'toRational' on its domain of definition,
          i.e. @fromRationalMaybe (toRational i) == Just i@ and @fromRationalMaybe i == Just j ==> toRational j = i@
  -}
  fromRationalMaybe :: Rational -> Maybe a

realToFracMaybe :: (Real a, SafeFractional b) => a -> Maybe b
realToFracMaybe = fromRationalMaybe . toRational

-- | Implement 'fromRationalMaybe' by converting the numerator and the denominator to 'a's via 'fromIntegerMaybe'
-- and then using '\/'. Returns 'Nothing' for a 0 denominator. Not universally safe as it relies on '\/' being total
-- given non-zero denominators.
fromRationalViaInteger :: forall a . (Fractional a, SafeNum a) => Rational -> Maybe a
fromRationalViaInteger (n :% 0) = Nothing
fromRationalViaInteger (n :% d) = (/) <$> fromIntegerMaybe n <*> fromIntegerMaybe d

-- This instance is only sensible if 'Ratio a' is generally well-behaved,
-- which it often isn't...
instance (Integral a, SafeNum a) => SafeFractional (Ratio a) where
  fromRationalMaybe = fromRationalViaInteger
