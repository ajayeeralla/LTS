{-# LANGUAGE ScopedTypeVariables #-}

import Data.LTS

main = do
    let s0 :: LTSState Int = LTSState {stateId=0, out=3}
    let s1 :: LTSState Int = LTSState {stateId=1, out=5}
    let s2 :: LTSState Int = LTSState {stateId=2, out=7}
    let t1 :: Transition Int Char = Transition {transitionFrom=s0, transitionGuard='a', transitionTo=s1}
    let t2 :: Transition Int Char = Transition {transitionFrom=s1, transitionGuard='b', transitionTo=s2}

    putStrLn "depth of LTS [t1, t2]:"
    print (depth [t1, t2] s0)
