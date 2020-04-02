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

module Data.LTS
  (LTSState (..)
  , Alphabet
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
  , deterministicLTS) where
import Data.Nat
import Data.List (sortBy)
import Data.Ord (comparing)

-- | LTSState is a record type which may hold id, output, etc.
data LTSState a =
  LTSState {stateId::Int
           , output::a
           }
           deriving (Read, Show, Eq)

-- | Define Ord instance by id
instance (Eq a)=> Ord (LTSState a) where
  compare = comparing stateId

-- | Alphabet can be a list of any type
type Alphabet a = [a]

-- | Transition models that on a LTSState, given input takes a step to the next LTSState
data Transition a b =
  Transition { transitionFrom::LTSState a
             , transitionGuard::b
             , transitionTo::LTSState a
             }
             deriving (Read, Show, Eq)

-- | Define Ord instance
instance (Eq a, Eq b) => Ord (Transition a b) where
  compare = comparing transitionFrom

-- | LTS is nothing but list of transitions
type LTS a b = [Transition a b]

-- | Check if the set of transitions has same origin
checkTrans :: (Eq a, Eq b) => LTSState a -> LTS a b -> Bool
checkTrans st (t:ts) =
  stateId st == stateId (transitionFrom t) && checkTrans st  ts

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

-- | Set the following for deterministic behaviour of a LTS
deterministicLTS::Bool
deterministicLTS= True

-- | Compute set of transitions from a given LTSState
collectTrans:: (Eq a, Eq b) => LTSState a  -> LTS a b -> LTS a b
collectTrans st (t:ts) =
  let op = if stateId st == stateId (transitionFrom t)
              then t:collectTrans st ts
              else collectTrans st ts in
              if deterministicLTS then sortByToSt op else op


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
