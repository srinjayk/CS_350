import System.IO
import Control.Monad

choose n 0 = 1
choose 0 k = 0
choose n k = choose (n-1) (k-1) * n `div` k 

fac 0 = 1
fac n = n * fac (n-1)

lucky 0 = "one"
lucky 1 = "two"
lucky 2 = "three"
lucky x = "nothing"

name 'a' = "Albert"
name 'b' = "Bernard"

addvectors a b = (fst a + fst b, snd a + snd b)

first (x, _, _) = x

part x 
     | x<4 = "the number is less than 4"
     | x<7 && x>5 = "between 7 and 5"
     | otherwise = "larger number"

cusmax a b
     | a>b = a
     | otherwise = b

a `myCompare` b
     | a>b = "GT"
     | b>a = "LT"
     | otherwise = "EQ"

part2 x 
     | sq<47 = "the number is less than 4"
     | sq<74 && sq>54 = "between 7 and 5"
     | otherwise = "larger number"
     where sq = x*x

-- pi = 3.14

cylinder r h = 
      let sideArea = 2 * pi * r * h  
          topArea = pi * r ^2  
      in  sideArea + 2 * topArea 

ls1 = [if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]  


-- describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 

describeList1 xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."

maxm1 [] = 0
maxm1 (x:xs) = max x (maxm1 xs)

cusreplicate n x 
      | n <= 0 = []
      | otherwise = x: (cusreplicate (n-1) x)

cusreverse [] = []
cusreverse (x:xs) = (cusreverse xs) ++ [x]

custake n _ 
      | n <= 0 = []
custake _ [] = []
custake n (x:xs) = x:custake (n-1) xs

-- cusdrop n _ 
--       | n <= 0 = []
-- cusdrop _ [] = []
-- cusdrop n l = (cusdrop (n-1) (init l))  ++ (last l)

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


applytwice f x = f (f x)

addtwo a b = a+b

cuszipwith f _ [] = []
cuszipwith f [] _ = []
cuszipwith f (x:xs) (y:ys) = (f x y):(cuszipwith f xs ys)

cusmap f [] = []
cusmap f (x:xs) = (f x):(cusmap f xs)

cusfilter _ [] = []
cusfilter f (x:xs)
      | f x = (x):(cusfilter f xs)
      | otherwise = (cusfilter f xs)

chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  


cusand a b = a && b
cusor a b = a || b

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

neighbors1 :: (Ord a1, Ord a2, Num a1, Num a2) => a1 -> a2 -> [(a1, a2)]
neighbors1 x y
     | x==0 && y==0 = [(0,1),(1,0),(1,1)]
     | x==9 && y==9 = [(8,8),(8,9),(9,8)]
     | x==0 && y==9 = [(0,8),(1,8),(1,9)]
     | x==9 && y==0 = [(8,0),(8,1),(9,1)]
     | x==0 = [(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]
     | y==0 = [(x-1,y),(x-1,y+1),(x,y+1),(x+1,y),(x+1,y+1)]
     | x==9 = [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1)]
     | y==9 = [(x-1,y-1),(x-1,y),(x,y-1),(x+1,y-1),(x+1,y)]
     | otherwise = [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]

getneighbors x y = [(x-1,y-1),(x-1,y),(x-1,y+1),(x,y-1),(x,y+1),(x+1,y-1),(x+1,y),(x+1,y+1)]
neighbors :: (Ord a1, Ord a2, Num a1, Num a2) => a1 -> a2 -> [(a1, a2)]
neighbors x y = [(a,b) | (a,b)<-(getneighbors x y), a>=0 , a<=9, b>=0, b<=9]

compose_multiple [] a = a;
compose_multiple [x] a = (x a)
compose_multiple (x:xs) a = x (compose_multiple xs a)
-- compose (x:xs) a = 

rembackn [] = []
rembackn (x:xs) 
    | x=='\n' = ' ':(rembackn xs)
    | otherwise = x:(rembackn xs)

rempunc [] = []
rempunc (x:xs)
    | x=='.' || x==',' || x==';' || x=='?' = ' ':(rempunc xs)
    | otherwise = x:(rempunc xs)

countwordsutil [] a
    | a==1 = 1
    | otherwise = 0
countwordsutil (x:xs) a
    | x/=' ' = countwordsutil xs 1
    | otherwise = a + (countwordsutil xs 0)
countwords1 l = countwordsutil l 0

-- contunique [] = []
-- contunique l
--     | length l <= 1 = l
--     | otherwise = if (head l)==(head (tail l)) then (head l):(contunique (tail (tail l))) else (head l):(contunique (tail l))

cnt = do  
    handle <- openFile "hello.txt" ReadMode  
    contents <- hGetContents handle  
    print contents  
    print (countwords (rempunc (rembackn contents)))
    hClose handle 

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show
leaf x = Node x Empty Empty

tree1 = Node 1 (Node 2 (leaf 3)
                                (leaf 4))
                    (Node 5 Empty
                                (Node 6 (leaf 6)
                                            Empty))

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


foldtreeold f id Empty = id
foldtreeold f id (Node a l r) = foldtreeold f (f id (foldtreeold f a l)) r

foldtree f id Empty = id
-- foldtreenew f id (Node a l r) = foldtreenew f (f id (foldtreenew f a l)) r
foldtree f id (Node a l r) = (f a (foldtree f id l) (foldtree f id r))

cusmult a b c = a*b*c
cusadd a b c = a+b+c

mapto1 [] = []
mapto1 (x:xs) = [1] ++ mapto1(xs)

countwords l = foldr (+) 0 (mapto1 (words l))