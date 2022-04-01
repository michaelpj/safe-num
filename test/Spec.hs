{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main (main) where

import SafeNum

import Test.Tasty
import Test.QuickCheck
import Test.QuickCheck.Instances.Natural ()
import Data.Proxy
import Control.Exception
import Data.Either (isRight)
import Data.Maybe (isJust)
import Test.Tasty.QuickCheck
import Numeric.Natural
import Data.Typeable
import Data.Word
import Data.Int
import Data.Ratio
import GHC.Real
import Data.Complex

-----------
-- Integral
-----------

prop_toIntegerTotal :: forall a . (Show a, Arbitrary a, Integral a) => Proxy a -> Property
prop_toIntegerTotal _ = forAll arbitrary $ \(i :: a) -> ioProperty $ do
  i' <- try @SomeException $ evaluate $ toInteger i
  pure $ isRight i'

prop_fromIntegerToIntegerLeftInverse :: forall a . (Show a, Arbitrary a, Integral a) => Proxy a -> Property
prop_fromIntegerToIntegerLeftInverse _ = forAll arbitrary $ \(i :: a) -> fromInteger (toInteger i) === i

integralLaws :: forall a . (Typeable a, Show a, Arbitrary a, Integral a) => Proxy a -> TestTree
integralLaws p = testGroup ("Integral laws for " ++ show (typeRep p))
  [ testProperty "toInteger total" $ prop_toIntegerTotal p
  , testProperty "fromInteger/toInteger left inverse" $ prop_fromIntegerToIntegerLeftInverse p
  ]

----------
-- SafeNum
----------

prop_fromIntegerMaybeTotal :: forall a . (Show a, Arbitrary a, SafeNum a) => Proxy a -> Property
prop_fromIntegerMaybeTotal _ = forAll arbitrary $ \(i :: Integer) -> ioProperty $ do
  i' <- try @SomeException $ evaluate $ fromIntegerMaybe @a i
  pure $ isRight i'

prop_fromIntegerMaybeFromIntegerAgreement :: forall a . (Show a, Eq a, Arbitrary a, SafeNum a) => Proxy a -> Property
prop_fromIntegerMaybeFromIntegerAgreement _ = forAll arbitrary $ \(i :: Integer) ->
  case fromIntegerMaybe @a i of
    Just i' -> i' === fromInteger i
    Nothing -> discard

prop_fromIntegerMaybeToIntegerLeftInverse :: forall a . (Show a, Arbitrary a, SafeNum a, Integral a) => Proxy a -> Property
prop_fromIntegerMaybeToIntegerLeftInverse _ = forAll arbitrary $ \(i :: a) -> fromIntegerMaybe (toInteger i) === Just i

prop_fromIntegerMaybeToIntegerRightInverse :: forall a . (Show a, Arbitrary a, SafeNum a, Integral a) => Proxy a -> Property
prop_fromIntegerMaybeToIntegerRightInverse _ = forAll arbitrary $ \(i :: Integer) ->
  case fromIntegerMaybe @a i of
    Just i' -> toInteger i' === i
    Nothing -> discard

safeNumLaws :: forall a . (Typeable a, Show a, Arbitrary a, SafeNum a, Integral a) => Proxy a -> TestTree
safeNumLaws p = testGroup ("SafeNum laws for " ++ show (typeRep p))
  [ testProperty "fromIntegerMaybe total" $ prop_fromIntegerMaybeFromIntegerAgreement p
  , testProperty "fromIntegerMaybe/fromInteger agreement" $ prop_fromIntegerMaybeToIntegerLeftInverse p
  , testProperty "fromIntegerMaybe/toInteger left inverse " $ prop_fromIntegerMaybeToIntegerLeftInverse p
  , testProperty "fromIntegerMaybe/toInteger right inverse" $ prop_fromIntegerMaybeToIntegerRightInverse p
  ]

------------------
-- Real+Fractional
------------------

newtype Ratio' a = Ratio' (Ratio a)
  deriving newtype (Eq, Ord, Num, Real, Fractional, SafeFractional)

arbitraryRatio :: (Show a, Integral a) => Gen a -> Gen (Ratio a)
arbitraryRatio gen = do
    n <- gen
    d <- gen `suchThat` (\n -> n /= 0)
    pure $ n % d

instance (Show a, Integral a, Arbitrary a) => Arbitrary (Ratio' a) where
  arbitrary = Ratio' <$> (arbitraryRatio arbitrary)

instance Show a => Show (Ratio' a) where
  show (Ratio' (n :% d)) = show n ++ " :% " ++ show d

prop_toRationalTotal :: forall a . (Show a, Arbitrary a, Real a) => Proxy a -> Property
prop_toRationalTotal _ = forAll arbitrary $ \(i :: a) -> ioProperty $ do
  i' <- try @SomeException $ evaluate $ toRational i
  pure $ isRight i'

prop_fromRationalToRationalLeftInverse :: forall a . (Show a, Eq a, Arbitrary a, Real a, Fractional a) => Proxy a -> Property
prop_fromRationalToRationalLeftInverse _ = forAll arbitrary $ \(i :: a) ->
  fromRational (toRational i) === i

realLaws :: forall a . (Typeable a, Show a, Arbitrary a, Real a) => Proxy a -> TestTree
realLaws p = testGroup ("Real laws for " ++ show (typeRep p))
  [ testProperty "toRational total" $ prop_toRationalTotal p
  ]

realFractionalLaws :: forall a . (Typeable a, Show a, Arbitrary a, Real a, Fractional a) => Proxy a -> TestTree
realFractionalLaws p = testGroup ("Real+Frational laws (" ++ show (typeRep p) ++ ")")
  [ testProperty "fromRational/toRational left inverse" $ prop_fromRationalToRationalLeftInverse p
  ]

-----------------
-- SafeFractional
-----------------

prop_fromRationalMaybeTotal :: forall a . (Show a, Arbitrary a, SafeFractional a) => Proxy a -> Property
prop_fromRationalMaybeTotal _ = forAll arbitrary $ \(i :: Rational) -> ioProperty $ do
  i' <- try @SomeException $ evaluate $ fromRationalMaybe @a i
  pure $ isRight i'

prop_fromRationalMaybeFromRationalAgreement :: forall a . (Show a, Eq a, Arbitrary a, SafeFractional a) => Proxy a -> Property
prop_fromRationalMaybeFromRationalAgreement _ = forAll arbitrary $ \(i :: Rational) ->
  case fromRationalMaybe @a i of
    Just i' -> i' === fromRational i
    Nothing -> discard

prop_fromRationalMaybeToRationalLeftInverse :: forall a . (Show a, Arbitrary a, SafeFractional a, Real a) => Proxy a -> Property
prop_fromRationalMaybeToRationalLeftInverse _ = forAll arbitrary $ \(i :: a) -> fromRationalMaybe (toRational i) === Just i

prop_fromRationalMaybeToRationalRightInverse :: forall a . (Show a, Arbitrary a, SafeFractional a, Real a) => Proxy a -> Property
prop_fromRationalMaybeToRationalRightInverse _ = forAll arbitrary $ \(i :: Rational) ->
  case fromRationalMaybe @a i of
    Just i' -> toRational i' === i
    Nothing -> discard

safeFractionalLaws :: forall a . (Typeable a, Show a, Arbitrary a, SafeFractional a, Real a) => Proxy a -> TestTree
safeFractionalLaws p = testGroup ("SafeFractional laws for " ++ show (typeRep p))
  [ testProperty "fromRationalMaybe total" $ prop_fromRationalMaybeTotal p
  , testProperty "fromRationalMaybe/fromRational agreement" $ prop_fromRationalMaybeToRationalLeftInverse p
  , testProperty "fromRationalMaybe/toRational left inverse " $ prop_fromRationalMaybeToRationalLeftInverse p
  , testProperty "fromRationalMaybe/toRational right inverse" $ prop_fromRationalMaybeToRationalRightInverse p
  ]

--------
-- Tests
--------

integralTests :: TestTree
integralTests = testGroup "Integral tests"
  [ integralLaws (Proxy @Integer)
  , integralLaws (Proxy @Natural)
  , integralLaws (Proxy @Int)
  , integralLaws (Proxy @Int8)
  , integralLaws (Proxy @Int32)
  , integralLaws (Proxy @Int64)
  , integralLaws (Proxy @Word)
  , integralLaws (Proxy @Word8)
  , integralLaws (Proxy @Word16)
  , integralLaws (Proxy @Word32)
  , integralLaws (Proxy @Word64)
  ]

safeNumTests :: TestTree
safeNumTests = testGroup "SafeNum tests"
  [ safeNumLaws (Proxy @Integer)
  , safeNumLaws (Proxy @Natural)
  , safeNumLaws (Proxy @Int)
  , safeNumLaws (Proxy @Int8)
  , safeNumLaws (Proxy @Int16)
  , safeNumLaws (Proxy @Int32)
  , safeNumLaws (Proxy @Int64)
  , safeNumLaws (Proxy @Word)
  , safeNumLaws (Proxy @Word8)
  , safeNumLaws (Proxy @Word16)
  , safeNumLaws (Proxy @Word32)
  , safeNumLaws (Proxy @Word64)
  ]

realTests :: TestTree
realTests = testGroup "Real tests"
  [ realLaws (Proxy @(Ratio' Integer))
  , realLaws (Proxy @(Ratio' Int))
  , realLaws (Proxy @Double)
  , realLaws (Proxy @Float)
  , realLaws (Proxy @Integer)
  , realLaws (Proxy @Natural)
  , realLaws (Proxy @Int)
  , realLaws (Proxy @Int8)
  , realLaws (Proxy @Int16)
  , realLaws (Proxy @Int32)
  , realLaws (Proxy @Int64)
  , realLaws (Proxy @Word)
  , realLaws (Proxy @Word8)
  , realLaws (Proxy @Word16)
  , realLaws (Proxy @Word32)
  , realLaws (Proxy @Word64)
  ]

realFractionalTests :: TestTree
realFractionalTests = testGroup "Real+Fractional tests"
  [ realFractionalLaws (Proxy @(Ratio' Integer))
  , realFractionalLaws (Proxy @(Ratio' Int))
  , realFractionalLaws (Proxy @Double)
  , realFractionalLaws (Proxy @Float)
  ]

safeFractionalTests :: TestTree
safeFractionalTests = testGroup "SafeFractional tests"
  [ safeFractionalLaws (Proxy @(Ratio' Integer))
  , safeFractionalLaws (Proxy @(Ratio' Int))
  ]

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 2000) $ testGroup "safe-num"
  [ testGroup "Existing classes"
    [ integralTests
    , realTests
    , realFractionalTests
    ]
  , testGroup "Safe classes"
    [ safeNumTests
    , safeFractionalTests
    ]
  ]
