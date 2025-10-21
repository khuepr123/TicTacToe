import Control.Monad.State

main :: IO ()
main = runStateT code [1..] >> return ()
--
-- layer an infinite list of uniques over the IO monad
--

code :: StateT [Integer] IO ()
code = do
    x <- pop
    io $ print x
    y <- pop
    io $ print y
    return ()

--
-- pop the next unique off the stack
--
pop :: StateT [Integer] IO Integer
pop = do
    (x:xs) <- get
    put xs
    return x

io :: IO a -> StateT [Integer] IO a
io = liftIO

--
-- another example: a guessing game 
-- (from http://scsibug.com/2006/11/28/a-simple-game-with-statet/)
--

module Main where
import System.Random
import Control.Monad.State

main = do answer <- getStdRandom (randomR (1,100)) -- think of a number
          putStrLn "I'm thinking of a number between 1 and 100, can you guess it?"
          guesses <- execStateT (guessSession answer) 0
          putStrLn $ "Success in " ++ (show guesses) ++ " tries."

guessSession :: Int -> StateT Int IO ()
guessSession answer =
    do gs <- lift getLine    -- get guess from user
       let g = read gs       -- convert to number
       modify (+1)           -- increment number of guesses
       case compare g answer of
              LT -> do lift $ putStrLn "Too low"
                       guessSession answer
              GT -> do lift $ putStrLn "Too high"
                       guessSession answer
              EQ -> lift $ putStrLn "Got it!"


--
-- another example: a global state 
-- (corrected from http://www.haskell.org/hawiki/MonadState?action=show)
--

import Control.Monad.State

data Vars = Vars {
   var1 :: Int,
   var2 :: Float
}

type MyState a = StateT Vars IO a
type Selector a = (MyState a, a -> MyState ())

s1 :: Selector Int
s1 = (gets var1, \x -> modify (\vs -> vs {var1 = x}))

s2 :: Selector Float
s2 = (gets var2, \x -> modify (\vs -> vs {var2 = x}))

sel :: Selector a -> MyState a
sel = fst

mods :: Selector a -> (a -> a) -> MyState ()
mods (gf,uf) mfun = do st <- gf
                       uf (mfun st)

sample :: MyState ()
sample = do a <- sel s1
            mods s2 (\x -> x * (fromIntegral a))
            b <- sel s2
            liftIO $ print b

main = runStateT sample (Vars 2 1.3)
