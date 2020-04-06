{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Main
  (prop_sortById
  , main) where
import Test.QuickCheck
import Data.LTS


-- | This is obviously true
prop_sortById (xs:: [LTSState Int]) =  sortById (sortById xs) == sortById xs


instance Arbitrary (LTSState Int) where
  arbitrary = elements [LTSState {stateId=0, Data.LTS.output=1}]

main :: IO ()
main = quickCheck prop_sortById
