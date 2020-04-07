{-# LANGUAGE BangPatterns #-}

module Main
  ( main
  )
where
import Criterion.Main
import Data.LTS

main =
  let s0 = LTSState {stateId=0, out=3}
  let s1 = LTSState {stateId=1, out=5}
  let s2 = LTSState {stateId=2, out=7}
  let t1 = Transition {transitionFrom=s0, transitionGuard=True, transitionTo=s1}
  let t2 = Transition {transitionFrom=s1, transitionGuard=True, transitionTo=s2}
  let !r = depth [t1, t2]
  defaultMain [
    bgroup "compTrans" [ bench "1" $ whnf Tran]
  putStrLn $ "depth took about " ++ show (diffUTCTime end start)
