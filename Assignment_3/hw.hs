-- Srinjay Kumar
-- 170722
-- CS 350A Assignment 3


import System.IO
import Control.Monad


-- Q!
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


-- Q2
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



-- Q3

getneighbors x y = [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]
neighbors :: (Ord a1, Ord a2, Num a1, Num a2) => a1 -> a2 -> [(a1, a2)]
neighbors x y = [(a,b) | (a,b)<-(getneighbors x y), a>=0 , a<=9, b>=0, b<=9]






-- compose (x:xs) a = 
-- Q4

mapto1 [] = []
mapto1 (x:xs) = [1] ++ mapto1(xs)

countwords l = foldr (+) 0 (mapto1 (words l))






-- Q5
compose_multiple [] a = a;
compose_multiple [x] a = (x a)
compose_multiple (x:xs) a = x (compose_multiple xs a)




-- Q6 
-- a
data BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving Show

tree1 = Node 1 (Node 2 (Node 3 Empty Empty)
                                (Node 4 Empty Empty))
                    (Node 5 Empty
                                (Node 6 (Node 7 Empty Empty)
                                            Empty))
 
 -- b
getsum Empty = 0
-- getsum (leaf a) = a
getsum (Node a l r) = a + (getsum l) + (getsum r)

maptree f Empty = Empty
maptree f (Node a l r) = Node (f a) (maptree f l) (maptree f r)

levelorder Empty = []
levelorder (Node a l r) = (levelorder l) ++ [a] ++ (levelorder r)

preorder Empty = []
preorder (Node a l r) = [a] ++ (preorder l) ++ (preorder r)

postorder Empty = []
postorder (Node a l r) = (postorder l) ++ (postorder r) ++ [a]

-- c

foldtreeold f id Empty = id
foldtreeold f id (Node a l r) = foldtreeold f (f id (foldtreeold f a l)) r

foldtree f id Empty = id
-- foldtreenew f id (Node a l r) = foldtreenew f (f id (foldtreenew f a l)) r
foldtree f id (Node a l r) = (f a (foldtree f id l) (foldtree f id r))

cusmult a b c = a*b*c
cusadd a b c = a+b+c
