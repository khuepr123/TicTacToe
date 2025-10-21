module Human (playHuman) where

import BackEnd
import Control.Monad.State
import Text.Read(readMaybe)
import System.IO

readTileIndex :: IO TileIndex
readTileIndex = do
    putStr "Enter board coordinate: (0-indexed, in format \"row column\")\n"
    hFlush stdout
    line <- getLine 
    case words line of
        [x, y] | Just a <- readMaybe x, Just b <- readMaybe y -> return (a, b)
        _ -> putStrLn "Invalid! Enter exactly two integers." >> readTileIndex
    

io :: IO a -> StateT GameState IO a
io = liftIO


playHuman :: Player -> StateT GameState IO ()
playHuman player = do
    tID         <- io $ readTileIndex
    errorString <- convert $ playMove player tID
    case errorString of
      Nothing -> return ()
      Just s  -> do
          io $ print s
          playHuman player
