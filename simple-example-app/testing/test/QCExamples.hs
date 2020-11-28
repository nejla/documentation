{-# LANGUAGE NoMonomorphismRestriction #-}
module QCExample where

import Test.QuickCheck

import Lib (frobnicate)

check :: Testable prop => prop -> IO ()
check = quickCheckWith args
  where
    args = stdArgs { maxSize = 1000}

prop_FrobnicateIncreasing :: Int -> Bool
prop_FrobnicateIncreasing x =
  frobnicate x > x


prop_FrobnicateSmall :: Int -> Bool
prop_FrobnicateSmall x = frobnicate x < 100

prop_FrobnicateSmallDiag :: Int -> Property
prop_FrobnicateSmallDiag x = collect x $
    frobnicate x < 100

prop_FrobnicateSmallChecked :: Int -> Property
prop_FrobnicateSmallChecked x = cover (x > 100) 10 "large" $
    frobnicate x < 100

prop_frobnicate_preserves_even x =
  if even x
  then even (frobnicate x)
  else odd (frobnicate x)
