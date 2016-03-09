module Queue where

import Control.Monad.State

data Queue a = Queue ([a], [a]) deriving Show

empty :: Queue a 
empty = Queue ([],[])

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue (xs, ys)) = Queue (xs, x:ys)

deque :: Queue a -> (a, Queue a)
deque (Queue ([],[]))    = undefined
deque (Queue ([], ys))   = deque $ Queue (reverse ys, [])
deque (Queue (x:xs, ys)) = (x, Queue (xs,ys))

type QVal   = Maybe Int
type QState = Queue Int 
type QStore = State QState QVal

start :: QState
start = empty

isEmpty :: Queue a -> Bool
isEmpty (Queue ([], [])) = True
isEmpty _                = False

runQueue :: QVal -> QStore
runQueue Nothing  = 
            do q <- get
               if isEmpty q 
                 then return Nothing
                 else let (v, q') = deque q 
                      in do put q' 
                            return $ Just v
               
runQueue (Just x) = 
            do q <- get
               put (enqueue x q)
               return Nothing

main :: IO ()
main = qmain start
    where 
    qmain :: QState -> IO ()
    qmain s = do line <- getLine
                 case line of
                     "quit" -> return ()
                     "show" -> print s >> qmain s
                     "deq"  -> let (v,q) = runState (runQueue Nothing) s
                               in case v of
                                    Nothing -> putStrLn "empty queue" >> qmain q
                                    Just x  -> print x >> qmain q
                     _     -> case reads line :: [(Int, String)] of
                                 []          -> qmain s
                                 ((x,_):_)   -> let (_,q) = runState (runQueue $ Just x) s
                                                in print q >> qmain q
