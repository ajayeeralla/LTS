{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main
where
import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit
import Data.LTS
import GHC.Generics
import Test.SmallCheck.Series


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

instance Serial m a => Serial m (LTSState a)
instance (Serial m a, Serial m b) => Serial m (Transition a b)

scProps = testGroup "(checked by SmallCheck)"
    [ SC.testProperty "sortById == sortById . sortById" $
      \xs -> sortById (xs :: [LTSState Int]) == sortById (sortById xs)
    , SC.testProperty "sortByFromSt == sortByFromSt . sortByFromSt" $
      \xs -> sortByFromSt (xs :: LTS Int Bool) == sortByFromSt (sortByFromSt xs)
    , SC.testProperty "sortByToSt == sortByToSt . sortByToSt" $
      \xs -> sortByToSt (xs :: LTS Int Bool) == sortByToSt (sortByToSt xs)
    ]


qcProps = testGroup "(checked by QuickCheck)"
    [ SC.testProperty "sortById == sortById . sortById" $
        \xs -> sortById (xs :: [LTSState Int]) == sortById (sortById xs)
    , SC.testProperty "sortByFromSt == sortByFromSt . sortByFromSt" $
        \xs -> sortByFromSt (xs :: LTS Int Bool) == sortByFromSt (sortByFromSt xs)
    , SC.testProperty "sortByToSt == sortByToSt . sortByToSt" $
        \xs -> sortByToSt (xs :: LTS Int Bool) == sortByToSt (sortByToSt xs)
    ]

s0 :: LTSState Int = LTSState {stateId=0, out=3}
s1 :: LTSState Int = LTSState {stateId=1, out=5}
s2 :: LTSState Int = LTSState {stateId=2, out=7}
t1 = Transition {transitionFrom=s0, transitionGuard=True, transitionTo=s1}
t2 = Transition {transitionFrom=s1, transitionGuard=True, transitionTo=s2}

unitTests = testGroup "Unit tests"
  [ testCase "Depth comparison" $
      depth [t1, t2] s0 `compare` 2 @?= EQ
  , testCase "Compare transition index" $
      findTransIndex s0 True [t1, t2] `compare` 1 @?= LT
  , testCase "Check transition Exists" $
      transExists s1 True [t1, t2] `compare` True @?= EQ
  ]
