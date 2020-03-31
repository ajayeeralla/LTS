{- |
Module      :  Data.LTS
Description :  A library that models labelled transition system in Haskell
Copyright   :  Copyright (c) 2020 Ajay Kumar Eeralla

Maintainer  :  ajay.eeralla@gmail.com
Stability   :  experimental
Portability :  portable

This module implements a labelled transition system
-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}

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

  -- | Check if the set of transitions has same origin
  checkTrans :: (Eq a, Eq b) => State a -> LTS a b -> Bool
  checkTrans State {Data.LTS.id=x, output=s} (t:ts) = x==Data.LTS.id (from t) && checkTrans State {Data.LTS.id=x, output=s}  ts

  -- | Get origin state ids
  getFromIds:: (Eq a, Eq b) =>  LTS a b -> [Int]
  getFromIds  = map (Data.LTS.id . from)

  -- | Get final state ids
  getToIds :: (Eq a, Eq b) => LTS a b -> [Int]
  getToIds = map (Data.LTS.id . to)

  -- | Sort states by Id
  sortById :: (Eq a) => [State a] -> [State a]
  sortById = sortBy (comparing Data.LTS.id)

  -- | Sort transitions by from state
  sortByFromSt :: (Eq a, Eq b) => LTS a b -> LTS a b
  sortByFromSt = sortBy (comparing from)

  -- | Sort transitions by to State
  sortByToSt :: (Eq a, Eq b) => LTS a b -> LTS a b
  sortByToSt = sortBy (comparing to)

  -- | Compute set of transitions from a given state
  collectTrans:: (Eq a, Eq b) => State a  -> LTS a b -> LTS a b
  collectTrans State {Data.LTS.id=x, output=s} (t:ts) = if x == Data.LTS.id (from t)
                        then t:collectTrans State {Data.LTS.id=x, output=s}  ts
                        else collectTrans State {Data.LTS.id=x, output=s} ts

  -- | Get start state
  getStartSt :: (Eq a, Eq b) => LTS a b -> State a
  getStartSt ts =  from (head (sortByToSt (sortByFromSt ts)))

  -- | Get Final state
  getFinalSt :: (Eq a, Eq b) => LTS a b -> State a
  getFinalSt ts =  to (head (sortByToSt (sortByFromSt ts)))

  -- | Compute depth of a transition system which is nothing but a longest simple path from the start to a final state
  depth :: (Eq a, Eq b) => LTS a b -> State a -> Nat
  depth [] _ = 0
  depth (t:ts) st
    | from t == st && (from t /= to t) = 1 + depth ts (to t)
    | otherwise = depth ts st
