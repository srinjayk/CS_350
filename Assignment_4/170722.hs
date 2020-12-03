-- Srinjay Kumar
-- 170722
-- Assignment 4

-- Monadic laws

-- 1) return a >>= f = f a
-- 2) m >>= return = m
-- 3) m >>= f >>= g = m >>= (\x -> f x >>= g)

-- Q1

-- Monadic laws proof for Maybe monad

-- Maybe Monad Rules
-- 1) return x = Just x
-- 2) Nothing >>= g = Nothing
-- 3) Just x >>= g = g x

-- Proof for Law 1 --
-- LHS = 
-- 	return a >>= f
-- 	= Just a >> f
-- 	= f a = RHS

-- Proof for Law 2 --
-- m >>= return 
-- Case 1  m = Just x
-- LHS = 
-- 		m >>= return 
-- 		= Just x >>= return
-- 		= Just x = RHS
-- Case 2 m = Nothing
-- LHS = 
-- 		m >>= Nothing
-- 		= Nothing = RHS

-- Proof for Law 3 --
-- m >>= f >>= g
-- Case 1 m = Just x
-- LHS = 
-- 		Just x >>= f >>= g
-- 		= (Just x >>= f) >>= g
-- 		= (f x) >>= g
-- 		= g(f(x))
-- RHS = 
-- 		Just x >>= (\a -> f a >>= g)
-- 		= f (x) >>= g
-- 		= g(f(x))
-- Case 2 m = Nothing
-- LHS = Nothing >>= f >>= g
-- 		= Nothing
-- 		= Nothing >>= (\a -> f a >>= g) = RHS



-- Q2

-- Monadic laws proof for List Monad

-- List Monad Rules
-- 1) return x = [x]
-- 2) xs >>= g = concat $ map g xs

-- Proof for Law 1 --
-- LHS = 
-- 	return a >>= f
-- 	= [a] >>= f
-- 	= concat $ map f [a]
-- 	= f a  = RHS

-- Proof for Law 2 -- 
-- LHS = 
-- 	m >>= return 
-- 	= [m1,m2,..] >>= return
-- 	= concat $ map return [m1,m2,..]
-- 	= concat [[m1], [m2],..]
-- 	= [m1,m2,..]
-- 	= m = RHS

-- Proof for Law 3 --
-- LHS = 
-- 	m >>= f >>= g
-- 	= concat $ map (f >>= g) m
-- 	= concat $ map (f >>= g) [m1,m2,..]
-- 	= concat [(f >>= g m1), (f >>= g m2), .. ]
-- 	= concat [(concat $ map g f m1), (concat $ map g f m2), .. ]
-- RHS = 
-- 	m >>= (\x -> f x >>= g)
-- 	= concat $ map (\x -> f x >>= g) m
-- 	= concat $ map (\x -> f x >>= g) [m1, m2, ..]
-- 	= concat [(f m1 >>= g), (f m2 >>= g), .. ]
-- 	= concat [(concat $ map g f m1), (concat $ map g f m2), .. ]

-- Therefore, LHS = RHS










-- Q 3

import Control.Monad.State

replace a ind l = (take ind l) ++ [a] ++ (drop (ind+1) l)


fibs_state :: Int -> State [Integer] Integer
fibs_state n = get >>= \l -> if (l !! n) >= 0
                                     then return (l !! n)
                                     else
                                      do
                                        n1 <- fibs_state (n-1)
                                        n2 <- fibs_state (n-2)
                                        let n3 = n1 + n2
                                        state $ (\lis -> (n3,(replace n3 n lis)))

fibonacci n = evalState (fibs_state n) (0:1:1:(take (n-2) [-1,-1..]))


-- Q4

cntoint x = read x::Int

cntutilwithdo n = do  
    inp <- getLine  
    let num = read inp::Int
    if(num == -1)
    then return n
    else cntutilwithdo (n+num)

cntutilwithoutdo n = getLine >>= \num -> if (cntoint(num) == (-1)) then return n else cntutilwithoutdo ((cntoint num)+n)

-- Q4a

cntwithdo = cntutilwithdo 0

-- Q4b

cntwithoutdo = cntutilwithoutdo 0
