module BackEnd (GameState, Player(..), TileIndex, playMove, BoardState, getWinner, WinState(..), initState, convert) where

import Data.Array as A
import qualified Data.Ix as Ix
import Text.Printf
import Data.Maybe
import Data.List(intercalate)
import Control.Monad.State

data WinState = Pending | Draw | Won Player deriving Eq

data GameState = GameState BoardState Player

data Player = PlayerX | PlayerO deriving Eq

instance Show Player where
    show PlayerX = "Player X"
    show PlayerO = "Player O"

data TileState = Empty | HasX | HasO deriving Eq

instance Show TileState where
    show Empty   = " "
    show HasX = "X"
    show HasO = "O"

type TileIndex = (Int, Int)

type BoardState = Array TileIndex TileState


convert :: State GameState a -> StateT GameState IO a
convert stateComp = do
    s <- get
    let (result, newState) = runState stateComp s
    put newState
    return result

-- clever trick: to initialize the player, simply do "playMove {someone}"
initState :: GameState
initState = let boardRange = ((0, 0), (2, 2)) in GameState 
                (listArray boardRange [Empty | _ <- Ix.range boardRange])
                PlayerX

instance Show GameState where
    show (GameState board _) = 
        let ((uRow, uCol), (dRow, dCol)) = bounds board
            -- nRow = dRow - uRow + 1
            -- nCol = dCol - uCol + 1
            horizontalBar = interpose "+" ["---" | _ <- [uCol..dCol]]
            getCell row col = show $ board ! (row, col)
            rowDisplay row = (interpose "|" $ [" " ++ getCell row col ++ " " | col <- [uCol..dCol]]) ++ "\n"
            fullboard = interpose (horizontalBar ++ "\n") (map rowDisplay [uRow..dRow])
         in fullboard
        where interpose :: [a] -> [[a]] -> [a]
              interpose l ls = l ++ (intercalate l ls) ++ l

-- cell design:
-- +---+
-- | X |
-- +---+

getPlayerSymbol :: Player -> TileState
getPlayerSymbol PlayerX = HasX
getPlayerSymbol PlayerO = HasO

opponent :: Player -> Player
opponent PlayerX = PlayerO
opponent PlayerO = PlayerX

playMove :: Player -> TileIndex -> State GameState (Maybe String)
playMove player tileIndex = do
    gameState <- get
    case playMoveTest player gameState tileIndex of
      Left s         -> return $ Just s
      Right newState -> do
          put newState
          return Nothing

playMoveTest :: Player -> GameState -> TileIndex -> Either String GameState
playMoveTest player (GameState board currentPlayer) tileIndex
  | player /= currentPlayer 
  = Left "Not your turn. Wait for the other player"
  | not $ Ix.inRange (bounds board) tileIndex 
  = Left $ printf "Out of bound. try picking inside board range %s" (show $ bounds board)
  | board ! tileIndex /= Empty 
  = Left $ printf "tile %s is occupied. Try picking empty tiles" (show tileIndex)
  | otherwise 
  = Right $ GameState (board // [(tileIndex, getPlayerSymbol player)]) (opponent player)

getChains :: BoardState -> [[TileIndex]]
getChains board = let ((uRow, uCol), (dRow, dCol)) = bounds board
                      rows = [zip (repeat row) [uCol..dCol] | row <- [uRow..dRow]]
                      cols = [zip [uRow..dRow] (repeat col) | col <- [uCol..dCol]]
                      fallingDiag = zip [uRow..dRow] [uCol..dCol]
                      risingDiag = zip [uRow..dRow] [dCol,dCol-1..uCol]
                   in fallingDiag : risingDiag : (rows ++ cols)


getWinner :: State GameState WinState
getWinner = do
    (GameState board _) <- get
    let chains = getChains board 
        tileStateChains = map (map (board !)) chains
        winnerChains = map getChainWinner tileStateChains
    let winState = case listToMaybe $ catMaybes winnerChains of
                     Just PlayerX -> Won PlayerX
                     Just PlayerO -> Won PlayerO
                     Nothing -> if elem Empty (A.elems board)
                                   then Pending
                                   else Draw
    return winState
        where getChainWinner :: [TileState] -> Maybe Player
              getChainWinner l
                      | all (==HasX) l = Just PlayerX
                      | all (==HasO) l = Just PlayerO
                      | otherwise      = Nothing       


