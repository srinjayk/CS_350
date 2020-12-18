-- 170722

-- Q8

import Control.Monad.State

type GameState = ([Char],Int)

begin :: State GameState ()
begin = state $ (\(l,i) -> ((),(l,0)))

peek :: State GameState [Char]
peek = state $ (\(l,i) -> (([(l !! 0)]),(l,i)))

next :: State GameState [Char]
next = do
         (l,i) <- get
         -- l <- fst var
         -- i <- snd var
         if i >= ((length l) - 1) then 
            state $ (\(l,i) -> (("Nothing"),(l,i))) 
         else 
            state $ (\(l,i) -> (([(l !! i)]),(l,(i+1))))

isOver :: State GameState Bool
isOver = do
         (l,i) <- get
         -- l <- fst var
         -- i <- snd var
         if i >= length l then 
            state $ (\(l,i) -> ((True),(l,i))) 
         else 
            state $ (\(l,i) -> ((False),(l,i)))

main = do
        putStrLn $ show $ flip evalState ("hel",0) (
             do
               begin
               peek
               next
               next
               next
               -- isOver
               -- next
               -- isOver
               -- next
             )