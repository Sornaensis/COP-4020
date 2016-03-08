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
type QState = (Queue Int, QVal)
type QStore = State QState QVal

start :: QState
start = (empty, Nothing)

isEmpty :: Queue a -> Bool
isEmpty (Queue ([], [])) = True
isEmpty _                = False

runQueue :: QVal -> QStore
runQueue Nothing  = 
            do (q, _) <- get
               if isEmpty q 
                 then return Nothing
                 else let (v, q') = deque q 
                      in do put (q', Just v) 
                            return (Just v)
               
runQueue (Just x) = 
            do (q, _) <- get
               put (enqueue x q, Nothing)
               return Nothing

main :: IO ()
main = qmain start
    where 
    qmain :: QState -> IO ()
    qmain s = do line <- getLine
                 case line of
                     "quit" -> return ()
                     "show" -> let (q,_) = s in print q >> qmain s
                     "deq"  -> let s'@(q,v) = execState (runQueue Nothing) s
                               in case v of
                                    Nothing -> putStrLn "empty queue" >> qmain s'
                                    Just x  -> print x >> qmain s'
                     _     -> case reads line :: [(Int, String)] of
                                 []          -> qmain s
                                 ((x,_):_)   -> let s'@(q,_) = execState (runQueue $ Just x) s
                                                in print q >> qmain s'
