{- |
Module      :  Data.LTS
Description :  A library that models labelled transition system in Haskell
Copyright   :  Copyright (c) 2020 Ajay Kumar Eeralla

Maintainer  :  ajay.eeralla@gmail.com
Stability   :  experimental
Portability :  portable

This module implements a labelled transition system
-}
{-# LANGUAGE DeriveGeneric #-}

module Data.LTS
  ( LTSState (..)
  , Transition (..)
  , LTS
  , checkTrans
  , getFromIds
  , getToIds
  , sortById
  , sortByToSt
  , sortByFromSt
  , collectTrans
  , getStartSt
  , getFinalSt
  , depth
  , Alphabet
  , findTransIndex
  )
where
import Data.Nat
import GHC.Generics
import Data.List (sortBy)
import Data.Ord (comparing)

-- | LTSState is a record type which may hold id, output, etc.
data LTSState a =
  LTSState {stateId::Int
           , out::a
           }
           deriving (Read, Show, Eq, Generic)

-- | Define Ord instance by id
instance (Eq a)=> Ord (LTSState a) where
  compare = comparing stateId

-- | Transition models that on a LTSState, given input symbol from an alphabet [b], takes to the next LTSState
data Transition a b =
  Transition { transitionFrom::LTSState a
             , transitionGuard::b
             , transitionTo::LTSState a
             }
             deriving (Read, Show, Eq, Generic)

-- | Define Ord instance
instance (Eq a, Eq b) => Ord (Transition a b) where
  compare = comparing transitionFrom

-- | Alphabet: a generic list
type Alphabet b = [b]

-- | LTS is nothing but list of transitions
type LTS a b = [Transition a b]

-- | Check if transition exists from a given symbol and state
transExists :: (Eq a, Eq b) => LTSState a -> b -> LTS a b -> Bool
transExists st x (t:ts) =
  (transitionFrom t == st && transitionGuard t == x) || transExists st x ts

-- | take next step from start state
findTransIndex :: (Eq a, Eq b) => LTSState a -> b -> LTS a b -> Int
findTransIndex st x (t:ts) =
  if transExists st x (t:ts) then
  (if transitionFrom t == st && transitionGuard t == x
    then 0
    else 1+ findTransIndex st x ts)
    else -1
-- | single transition
nextStateSymbol :: (Eq a, Eq b) => LTSState a -> b -> LTS a b -> LTSState a
nextStateSymbol s x ts =
  if transExists s x ts then transitionTo (ts !! findTransIndex s x ts)
    else s
-- | LTS execution on a given state and Alphabet
nextStateAlphabet :: (Eq a, Eq b) => LTSState a -> Alphabet b -> LTS a b -> LTSState a
nextStateAlphabet s (x:xs) ts =
  nextStateAlphabet (nextStateSymbol s x ts) xs ts

-- | Check if the set of transitions has same origin
checkTrans :: (Eq a, Eq b) => LTSState a -> LTS a b -> Bool
checkTrans st (t:ts) =
  stateId st == stateId (transitionFrom t) && checkTrans st  ts
-- | Sorting related functions
-- | Get origin LTSState ids
getFromIds:: (Eq a, Eq b) =>  LTS a b -> [Int]
getFromIds = map (stateId . transitionFrom)

-- | Get final LTSState ids
getToIds :: (Eq a, Eq b) => LTS a b -> [Int]
getToIds = map (stateId . transitionTo)

-- | Sort LTSStates by Id
sortById :: (Eq a) => [LTSState a] -> [LTSState a]
sortById = sortBy (comparing stateId)

-- | Sort transitions by from LTSState
sortByFromSt :: (Eq a, Eq b) => LTS a b -> LTS a b
sortByFromSt = sortBy (comparing transitionFrom)

-- | Sort transitions by to LTSState
sortByToSt :: (Eq a, Eq b) => LTS a b -> LTS a b
sortByToSt = sortBy (comparing transitionTo)

-- | Compute set of transitions (that can be ordered using a flag b) from a given LTSState
collectTrans:: (Eq a, Eq b) => LTSState a  -> LTS a b -> Bool -> LTS a b
collectTrans st (t:ts) b =
  let op = if stateId st == stateId (transitionFrom t)
              then t:collectTrans st ts b
              else collectTrans st ts b in
              if b then sortByToSt op else op

-- | Get start LTSState
getStartSt :: (Eq a, Eq b) => LTS a b -> LTSState a
getStartSt ts =  transitionFrom (head (sortByToSt (sortByFromSt ts)))

-- | Get Final LTSState
getFinalSt :: (Eq a, Eq b) => LTS a b -> LTSState a
getFinalSt ts =  transitionTo (head (sortByToSt (sortByFromSt ts)))

-- | Compute depth of a transition system which is nothing but a longest simple path from the start to a final LTSState
depth :: (Eq a, Eq b) => LTS a b -> LTSState a -> Nat
depth [] _ = 0
depth (t:ts) st
  | transitionFrom t == st && (transitionFrom t /= transitionTo t) = 1 + depth ts (transitionTo t)
  | otherwise = depth ts st
