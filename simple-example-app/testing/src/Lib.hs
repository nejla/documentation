module Lib where

frobnicate :: Int -> Int
frobnicate x = go $ go x
  where
    go = succ
