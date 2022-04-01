{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main (main) where

import SafeNum

import Test.Tasty
import Test.QuickCheck
import Data.Proxy
import Control.Exception
import Data.Either (isRight)
import Data.Maybe (isJust)
import Test.Tasty.QuickCheck
import Numeric.Natural
import Data.Typeable
import Data.Word
import Data.Int

prop_toIntegerTotal :: forall a . (Show a, Arbitrary a, Integral a) => Proxy a -> Property
prop_toIntegerTotal _ = forAll arbitrary $ \(i :: a) -> ioProperty $ do
  i' <- try @SomeException $ evaluate $ toInteger i
  pure $ isRight i'

prop_fromIntegerToIntegerLeftInverse :: forall a . (Show a, Arbitrary a, Integral a) => Proxy a -> Property
prop_fromIntegerToIntegerLeftInverse _ = forAll arbitrary $ \(i :: a) -> fromInteger (toInteger i) == i

toIntegerLaws :: forall a . (Typeable a, Show a, Arbitrary a, Integral a) => Proxy a -> TestTree
toIntegerLaws p = testGroup ("toInteger laws (" ++ show (typeRep p) ++ ")")
  [ testProperty "toInteger total" $ prop_toIntegerTotal p
  , testProperty "fromInteger/toInteger left inverse" $ prop_fromIntegerToIntegerLeftInverse p
  ]

prop_fromIntegerMaybeTotal :: forall a . (Show a, Arbitrary a, SafeNum a) => Proxy a -> Property
prop_fromIntegerMaybeTotal _ = forAll arbitrary $ \(i :: Integer) -> ioProperty $ do
  i' <- try @SomeException $ evaluate $ fromIntegerMaybe @a i
  pure $ isRight i'

prop_fromIntegerMaybeFromIntegerAgreement :: forall a . (Show a, Eq a, Arbitrary a, SafeNum a) => Proxy a -> Property
prop_fromIntegerMaybeFromIntegerAgreement _ = forAll arbitrary $ \(i :: Integer) ->
  let r = fromIntegerMaybe @a i
  in cover 30 (isJust r) "in range" $ case r of
    Just i' -> i' == fromInteger i
    Nothing -> discard

prop_fromIntegerMaybeToIntegerLeftInverse :: forall a . (Show a, Arbitrary a, SafeNum a, Integral a) => Proxy a -> Property
prop_fromIntegerMaybeToIntegerLeftInverse _ = forAll arbitrary $ \(i :: a) -> fromIntegerMaybe (toInteger i) == Just i

prop_fromIntegerMaybeToIntegerRightInverse :: forall a . (Show a, Arbitrary a, SafeNum a, Integral a) => Proxy a -> Property
prop_fromIntegerMaybeToIntegerRightInverse _ = forAll arbitrary $ \(i :: Integer) ->
  let r = fromIntegerMaybe @a i
  in cover 30 (isJust r) "in range" $ case r of
    Just i' -> toInteger i' == i
    Nothing -> discard

fromIntegerMaybeLaws :: forall a . (Typeable a, Show a, Arbitrary a, SafeNum a, Integral a) => Proxy a -> TestTree
fromIntegerMaybeLaws p = testGroup ("toInteger laws (" ++ show (typeRep p) ++ ")")
  [ testProperty "fromIntegerMaybe total" $ prop_fromIntegerMaybeFromIntegerAgreement p
  , testProperty "fromIntegerMaybe/fromInteger agreement" $ prop_fromIntegerMaybeToIntegerLeftInverse p
  , testProperty "fromIntegerMaybe/ToInteger left inverse " $ prop_fromIntegerMaybeToIntegerLeftInverse p
  , testProperty "fromIntegerMaybe/toInteger right inverse" $ prop_fromIntegerMaybeToIntegerRightInverse p
  ]

toIntegerTests :: TestTree
toIntegerTests = testGroup "toInteger tests"
  [ toIntegerLaws (Proxy @Integer)
  , testGroup "Ints"
    [ toIntegerLaws (Proxy @Int)
    , toIntegerLaws (Proxy @Int8)
    , toIntegerLaws (Proxy @Int16) -- etc
    ]
  -- No arbitrary instance?
  --, toIntegerLaws (Proxy @Natural)
  , testGroup "Words"
    [ toIntegerLaws (Proxy @Word)
    , toIntegerLaws (Proxy @Word8)
    , toIntegerLaws (Proxy @Word16) -- etc
    ]
  ]

fromIntegerMaybeTests :: TestTree
fromIntegerMaybeTests = testGroup "fromIntegerMaybe tests"
  [ fromIntegerMaybeLaws (Proxy @Integer)
  , testGroup "Ints"
    [ fromIntegerMaybeLaws (Proxy @Int)
    --, fromIntegerMaybeLaws (Proxy @Int8)
    --, fromIntegerMaybeLaws (Proxy @Int16) -- etc
    ]
  -- No arbitrary instance?
  --, toIntegerLaws (Proxy @Natural)
  , testGroup "Words"
    [ --fromIntegerMaybeLaws (Proxy @Word)
    --, fromIntegerMaybeLaws (Proxy @Word8)
    --, fromIntegerMaybeLaws (Proxy @Word16) -- etc
    ]
  ]

main :: IO ()
main = defaultMain $ localOption (QuickCheckTests 2000) $ testGroup "safe-num"
  [ toIntegerTests
  , fromIntegerMaybeTests
  ]
