{-# LANGUAGE ScopedTypeVariables #-}

module Main where
import Criterion.Main
import Data.LTS

s0 :: LTSState Int = LTSState {stateId=0, out=3}
s1 :: LTSState Int = LTSState {stateId=1, out=5}
s2 :: LTSState Int = LTSState {stateId=2, out=7}
t1 :: Transition Int Char = Transition {transitionFrom=s0, transitionGuard='a', transitionTo=s1}
t2 :: Transition Int Char = Transition {transitionFrom=s1, transitionGuard='b', transitionTo=s2}

main = defaultMain [
  bgroup "Depth" [ bench "1"  $ whnf depth [t1]
                 , bench "5"  $ whnf depth [t1, t2]
                 ]
  ]
