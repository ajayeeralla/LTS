{-# LANGUAGE BangPatterns #-}

module Main (main) where
import Data.Time.Clock
import Data.LTS

main = do
  start <- getCurrentTime
  let s0 = LTSState {stateId=0, output=3}
  let s1 = LTSState {stateId=1, output=5}
  let s2 = LTSState {stateId=2, output=7}
  let t1 = Transition{transitionFrom=s0, transitionGuard=True, transitionTo=s1}
  let t2 = Transition{transitionFrom=s1, transitionGuard=True, transitionTo=s2}
  let !r = depth [t1, t2]
  end <- getCurrentTime
  putStrLn $ "depth took about " ++ show (diffUTCTime end start)
