## LTS: Labelled Transition System
[![Travis](https://api.travis-ci.com/ajayeeralla/LTS.svg?branch=master)](https://travis-ci.com/github/ajayeeralla/LTS)
[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](https://github.com/ajayeeralla/LTS/edit/master/LICENSE)

This is a library that implements a [labelled transition system](https://en.wikipedia.org/wiki/Transition_system) that can be either deterministic or non-deterministic.

## Example

Here is an example to use LTS library:

```
import Data.LTS

main = do
    let s0 :: LTSState Int = LTSState {stateId=0, out=3}
    let s1 :: LTSState Int = LTSState {stateId=1, out=5}
    let s2 :: LTSState Int = LTSState {stateId=2, out=7}
    let t1 :: Transition Int Char = Transition {transitionFrom=s0, transitionGuard='a', transitionTo=s1}
    let t2 :: Transition Int Char = Transition {transitionFrom=s1, transitionGuard='b', transitionTo=s2}

    putStrLn "depth of LTS [t1, t2]:"
    print (depth [t1, t2] s0)
```

