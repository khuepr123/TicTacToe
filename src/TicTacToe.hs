module TicTacToe (playTicTacToe) where

import BackEnd
import Human
import Text.Printf
import Control.Monad.State
import Text.Read(readMaybe)
import System.IO

data Controller = Human | RandomBot | MinimaxBot deriving (Show, Enum)
playerTypeSelection = [Human, RandomBot, MinimaxBot]

type Options a = (String, [a], String)

robustGetInt :: (Int -> Bool) -> IO Int
robustGetInt checker = do
    l <- getLine
    let errorString = " is not a valid choice. Please choose a number as specified"
        tryAgain = (putStrLn $ l ++ errorString) >> robustGetInt checker
    case readMaybe l of
      Nothing -> tryAgain
      Just num -> if checker num 
                     then return num
                     else tryAgain

askSelect :: Show a => Options a -> IO a
askSelect (startString, listOptions, endString) = 
    let prependNum = map alignNum [1..]
        appendOption = map show listOptions
        optionsWithNum = zipWith (++) prependNum appendOption
        lines = [startString] ++ optionsWithNum ++ [endString]
     in do
            putStr $ foldr1 (\s t -> s ++ "\n" ++ t) lines
            hFlush stdout
            n <- robustGetInt (`elem` [1..length listOptions])
            return $ listOptions !! (n - 1)
            
    where spaceFromLeft = 5
          alignNum x = let s = show x
                           spNum = spaceFromLeft - (length s)
                        in [' ' | _ <- [1..spNum]] ++ s ++ ") "
    

printBoard :: StateT GameState IO ()
printBoard = do
    board <- get
    liftIO $ putStrLn "Current Game Board:"
    liftIO $ putStrLn (show board)

controlToAction :: Controller -> Player -> StateT GameState IO () -> StateT GameState IO ()
controlToAction controller player cont = do
    printBoard
    case controller of
      Human -> playHuman player
      RandomBot -> error "not implemented yet"
      MinimaxBot -> error "not implemented yet"
    winState <- convert getWinner
    case winState of
      Pending -> cont
      other   -> printBoard >> (liftIO $ endGame other)
    


endGame :: WinState -> IO ()
endGame Pending = error "game currently playing, not ended yet"
endGame winState = putStrLn $ 
    case winState of 
      Draw -> "Game Draw."
      Won whom -> printf "%s won." (show whom)


playGame :: StateT GameState IO ()
playGame = do
    liftIO $ putStrLn "Welcome to TicTacToe game. Choose Who will play playerX and playerO"
    controlX <- lift . askSelect $ controllerSelect PlayerX
    controlO <- lift . askSelect $ controllerSelect PlayerO
    let actX = controlToAction controlX PlayerX actO
        actO = controlToAction controlO PlayerO actX
    actX
    
playTicTacToe :: IO ()
playTicTacToe = evalStateT playGame initState

controllerSelect :: Player -> Options Controller
controllerSelect player = 
    ( printf "Who would control %s?" (show player)
    , playerTypeSelection
    , "Your choice? "
    )


    
