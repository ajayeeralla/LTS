{- |
Module      :  Data.LTS
Description :  A library that models labelled transition system in Haskell
Copyright   :  Copyright (c) 2020 Ajay Kumar Eeralla

Maintainer  :  ajay.eeralla@gmail.com
Stability   :  experimental
Portability :  portable

This module implements a labelled transition system
-}



module Data.LTS where
  import Data.Nat
  import Data.List (sortBy)
  import Data.Ord (comparing)

  -- | State is a record type which may hold id, output, etc.
  data State a = State {id::Int, output::a} deriving (Read, Show, Eq)

  -- | Define Ord instance by id
  instance (Eq a)=> Ord (State a) where
    compare = comparing Data.LTS.id

  -- | Alphabet can be a list of any type
  type Alphabet a = [a]

  -- | Transition models that on a state, given input takes a step to the next state
  data Transition a b = Transition {from::State a
                            , guard::b
                            , to::State a}
                            deriving (Read, Show, Eq)

  -- | Define Ord instance
  instance (Eq a, Eq b) => Ord (Transition a b) where
    compare = comparing from

  -- | LTS is nothing but list of transitions
  type LTS a b = [Transition a b]

  -- | Compute set of transitions from a given state
  collectTrans:: (Eq a, Eq b) => State a  -> [Transition a b] -> [Transition a b]
  collectTrans State {Data.LTS.id=x, output=s} (t:ts) = if x == Data.LTS.id (from t)
                        then t:collectTrans State {Data.LTS.id=x, output=s}  ts
                        else collectTrans State {Data.LTS.id=x, output=s} ts

  -- | Check if the set of transitions has same origin
  checkTrans :: (Eq a, Eq b) => State a -> LTS a b -> Bool
  checkTrans State {Data.LTS.id=x, output=s} (t:ts) = x==Data.LTS.id (from t) && checkTrans State {Data.LTS.id=x, output=s}  ts

  -- | Compute depth of a transition system which is nothing but a longest simple path from the origin to a final state
  computeDepth :: (Eq a, Eq b) => [LTS a b] -> State a -> Nat
  computeDepth [] _ = 0
  computeDepth (t:ts)
   State {Data.LTS.id=x, output=s}
     | (from t)== to t =
       computeDepth ts State {Data.LTS.id=x, output=s}
     | to t == x = 1 + computeDepth []
     | otherwise = 1 +
       computeDepth (collectTrans (to t))
          State {Data.LTS.id=x, output=s}
