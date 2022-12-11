{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Day11
  ( part1
  , part2
  ) where
import Optics.Operators
import Optics
import Data.Map.Strict as M (Map, (!), update, fromList, elems)
import Data.List (sort)
import qualified Debug.Trace  as T (trace)

data Monkey
  = Monkey
  { _items :: [Integer]
  , _totalItems :: Integer
  , _operation :: Integer -> Integer
  , _test :: Integer -> Int
  }
makeLenses ''Monkey

instance Show Monkey where
  show m = "Monkey: {totalItems: " ++ show ( m ^. totalItems) ++ ", items: " ++ show (_items m) ++ "}"

part1 = multiplyMax . trace "Monkeys: " . simulateThrows $ 20
  where
    simulateThrows n = elems $ iterate (monkeyThrow True 0) (fromList $ zip [0..] baseMonkeys) !! n
    multiplyMax = product .  take 2 . reverse . sort . fmap (^. totalItems)

part2 = multiplyMax . simulateThrows $ 10000
  where
    simulateThrows n = elems $ iterate (monkeyThrow False 0) (fromList $ zip [0..] baseMonkeys) !! n
    multiplyMax = product .  take 2 . reverse . sort . fmap (^. totalItems)

-- https://brilliant.org/wiki/modular-arithmetic/

baseMonkeys =
  [ Monkey [63, 84, 80, 83, 84, 53, 88, 72] 0 (* 11)        (\a -> if a `isDivisibleBy` 13 then 4 else 7)
  , Monkey [67, 56, 92, 88, 84]             0 (+ 4)         (\a -> if a `isDivisibleBy` 11 then 5 else 3)
  , Monkey [52]                             0 (\o -> o * o) (\a -> if a `isDivisibleBy`  2 then 3 else 1)
  , Monkey [59, 53, 60, 92, 69, 72]         0 (+ 2)         (\a -> if a `isDivisibleBy`  5 then 5 else 6)
  , Monkey [61, 52, 55, 61]                 0 (+ 3)         (\a -> if a `isDivisibleBy`  7 then 7 else 2)
  , Monkey [79, 53]                         0 (+ 1)         (\a -> if a `isDivisibleBy`  3 then 0 else 6)
  , Monkey [59, 86, 67, 95, 92, 77, 91]     0 (+ 5)         (\a -> if a `isDivisibleBy` 19 then 4 else 0)
  , Monkey [58, 83, 89]                     0 (* 19)        (\a -> if a `isDivisibleBy` 17 then 2 else 1)
  ]

monkeyThrow reduceStress monkeyIndex values
  | noMoreMonkeys  = values
  | noItemsToThrow = monkeyThrow reduceStress (monkeyIndex + 1) values
  | otherwise      = monkeyThrow reduceStress monkeyIndex newValues
  where
    noMoreMonkeys = monkeyIndex >= length values
    noItemsToThrow = null $ currentMonkey ^. items

    currentMonkey = values ! monkeyIndex
    currentItem = head $ currentMonkey ^. items
    updatedItem = reduceFunction (currentMonkey ^. operation $ currentItem)
    targetMonkey = currentMonkey ^. test $ updatedItem
    reduceFunction
      | reduceStress = (`div` 3)
      | otherwise = id


    newValues = trace "After Throw" $ addItemToTargetMonekey . removeItemFromStack . increaseCounter $ values
    increaseCounter = update (Just . (totalItems %~ (+1))) monkeyIndex
    removeItemFromStack = update (Just . (items %~ tail)) monkeyIndex 
    addItemToTargetMonekey = update (Just . (items %~ (++[updatedItem]))) targetMonkey
    

isDivisibleBy a n = a `mod` n == 0

-- trace t v = T.trace (t ++ show v) v
trace _ v = v