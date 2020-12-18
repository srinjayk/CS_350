-- 170722

import System.IO
import Control.Monad
import Data.Char

-- Q4

greaterlist [] pivot = []
greaterlist (x:xs) pivot
          | x>pivot = [x] ++ (greaterlist xs pivot)
          | otherwise = greaterlist xs pivot

smallerlist [] pivot = []
smallerlist (x:xs) pivot
          | x<pivot = [x] ++ (smallerlist xs pivot)
          | otherwise = smallerlist xs pivot

equallist [] pivot = []
equallist (x:xs) pivot
          | x==pivot = [x] ++ (equallist xs pivot)
          | otherwise = equallist xs pivot

quicksort [] = []
quicksort (x:xs) = (quicksort (smallerlist xs x)) ++ [x] ++ (quicksort (equallist xs x)) ++ (quicksort (greaterlist xs x))

isPermutation xs ys = do
                        let a = quicksort xs
                        let b = quicksort ys
                        if a==b then True else False

-- Q5

cusor a b = a||b
checkexistence _ [] = False
checkexistence b (y:ys)
     | b==y = True
     | otherwise = cusor False (checkexistence b ys)

cusunique _ [] = []
cusunique [] (x:xs) = [x] ++ (cusunique [x] xs)
cusunique a (x:xs)
     | checkexistence x a = cusunique a xs
     | otherwise = [x] ++ (cusunique (a ++ [x]) xs)

uniq a = cusunique [] a

delchar _ [] = []
delchar a (y:ys) = if a==y then ys else [y] ++ (delchar a ys)

permuteutil [] = [[]]
permuteutil l = [ x:z | x<-l , z<-permuteutil(delchar x l)]

genPermutations l = uniq (quicksort (permuteutil l))

-- Q6

countlines [] _ = []
countlines _ 0 = []
countlines (x:xs) a 
     | x=='\n' = [x] ++ (countlines xs (a-1))
     | otherwise = [x] ++ (countlines xs a)

firstLines a b = do
                    handle <- openFile a ReadMode
                    contents <- hGetContents handle
                    putStr $ countlines contents b
                    hClose handle