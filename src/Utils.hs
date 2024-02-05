module Utils ( splitList, replaceList, allM ) where

import Data.List ( stripPrefix, intercalate )

breakList :: Eq a => [a] -> [a] -> ([a], Maybe [a])
breakList [] _ = error "Invalid argument."
breakList _ [] = ([], Nothing)
breakList subStr str =
  case stripPrefix subStr str of
    Just x -> ([], Just x)
    Nothing ->
      (head str : l, r)
      where (l, r) = breakList subStr $ tail str


splitList :: Eq a => [a] -> [a] -> [[a]]
splitList subStr str =
  case breakList subStr str of
    (a, Nothing) -> [a]
    (a, Just b) -> a : splitList subStr b

replaceList :: Eq a => [a] -> [a] -> [a] -> [a]
replaceList subStr replacement =
  intercalate replacement . splitList subStr

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ [] = return True
allM f (x : xs) = do
  b <- f x
  if b then allM f xs else return False
