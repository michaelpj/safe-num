{-# LANGUAGE MagicHash #-}
module SafeNum where

import GHC.Num.Integer
import GHC.Int
import GHC.Real
import Data.Ratio

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

instance SafeNum Integer where
  fromIntegerMaybe i = Just i

instance SafeNum Int where
  fromIntegerMaybe i = case i of
    IS i -> Just (I# i)
    _ -> Nothing

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

instance (Integral a, SafeNum a) => SafeFractional (Ratio a) where
  fromRationalMaybe (n :% d) = (:%) <$> fromIntegerMaybe n <*> fromIntegerMaybe d
