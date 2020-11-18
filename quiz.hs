checkocc _ [] = False
checkocc a (x:xs)
     | a==x = True
     | otherwise = checkocc a xs

-- spellutil [] dict = []
spellutil list dict = [x | x<-list, checkocc x dict == False]

spell l dict = spellutil (words l) dict


data NL a = Num a | List [NL a] deriving Show

list1 = List [ Num 1, List [Num 1], Num 1]