-- 170722

-- Q7

import System.IO
import Control.Monad
import Data.Char

data Suit = Hearts | Clubs | Diamonds | Spades deriving (Show,Eq)
data Rank = Numbered Int | Jack | Queen | King | Ace deriving (Show,Eq)
data Card = Card Rank Suit | Joker deriving (Show,Eq)

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

getval a 
     | a == Card King Clubs = [13]
     | a == Card Queen Clubs = [12]
     | a == Card Jack Clubs = [11]
     | a == Card Ace Clubs = [20]
     | a == Card King Hearts = [13]
     | a == Card Queen Hearts = [12]
     | a == Card Jack Hearts = [11]
     | a == Card Ace Hearts = [20]
     | a == Card King Diamonds = [13]
     | a == Card Queen Diamonds = [12]
     | a == Card Jack Diamonds = [11]
     | a == Card Ace Diamonds = [20]
     | a == Card King Spades = [13]
     | a == Card Queen Spades = [12]
     | a == Card Jack Spades = [11]
     | a == Card Ace Spades = [20]
     | a == Card (Numbered 1) Spades = [1]
     | a == Card (Numbered 2) Spades = [2]
     | a == Card (Numbered 3) Spades = [3]
     | a == Card (Numbered 4) Spades = [4]
     | a == Card (Numbered 5) Spades = [5]
     | a == Card (Numbered 6) Spades = [6]
     | a == Card (Numbered 7) Spades = [7]
     | a == Card (Numbered 8) Spades = [8]
     | a == Card (Numbered 9) Spades = [9]
     | a == Card (Numbered 10) Spades = [10]
     | a == Card (Numbered 1) Clubs = [1]
     | a == Card (Numbered 2) Clubs = [2]
     | a == Card (Numbered 3) Clubs = [3]
     | a == Card (Numbered 4) Clubs = [4]
     | a == Card (Numbered 5) Clubs = [5]
     | a == Card (Numbered 6) Clubs = [6]
     | a == Card (Numbered 7) Clubs = [7]
     | a == Card (Numbered 8) Clubs = [8]
     | a == Card (Numbered 9) Clubs = [9]
     | a == Card (Numbered 10) Clubs = [10]
     | a == Card (Numbered 1) Diamonds = [1]
     | a == Card (Numbered 2) Diamonds = [2]
     | a == Card (Numbered 3) Diamonds = [3]
     | a == Card (Numbered 4) Diamonds = [4]
     | a == Card (Numbered 5) Diamonds = [5]
     | a == Card (Numbered 6) Diamonds = [6]
     | a == Card (Numbered 7) Diamonds = [7]
     | a == Card (Numbered 8) Diamonds = [8]
     | a == Card (Numbered 9) Diamonds = [9]
     | a == Card (Numbered 10) Diamonds = [10]
     | a == Card (Numbered 1) Hearts = [1]
     | a == Card (Numbered 2) Hearts = [2]
     | a == Card (Numbered 3) Hearts = [3]
     | a == Card (Numbered 4) Hearts = [4]
     | a == Card (Numbered 5) Hearts = [5]
     | a == Card (Numbered 6) Hearts = [6]
     | a == Card (Numbered 7) Hearts = [7]
     | a == Card (Numbered 8) Hearts = [8]
     | a == Card (Numbered 9) Hearts = [9]
     | a == Card (Numbered 10) Hearts = [10]
     | a == Joker = [11,12,13,20]

setadd a b = [ x+y | x<-a, y<-b]

setadd3 a b c = setadd a (setadd b c)

scores x = uniq (setadd3 (getval (x !! 0)) (getval (x !! 1)) (getval (x !! 2)))