module Tetris.Logic
    ( -- * Initialization
      initialState
    , emptyBoard
      -- * Event handling
    , handleEvent
      -- * Game mechanics
    , movePiece
    , rotatePiece
    , hardDrop
    , tick
      -- * Collision detection
    , checkCollision
    , getPieceBlocks
      -- * Board operations
    , lockPiece
    , clearLines
      -- * Scoring
    , calculateScore
    , levelSpeed
    ) where

import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))
import System.Random (StdGen, uniformR)

import Tetris.Types
import Tetris.Pieces

-- | Create an empty board
emptyBoard :: Board
emptyBoard = V.replicate boardHeight (V.replicate boardWidth Empty)

-- | Create initial game state with a random generator
initialState :: StdGen -> GameState
initialState rng =
    let (currentType, rng')  = randomPieceType rng
        (nextType, rng'')    = randomPieceType rng'
        currentPiece = Piece currentType 0 spawnPosition
    in GameState
        { gsBoard        = emptyBoard
        , gsCurrentPiece = currentPiece
        , gsNextPiece    = nextType
        , gsScore        = 0
        , gsLinesCleared = 0
        , gsLevel        = 1
        , gsGameOver     = False
        , gsRng          = rng''
        }

-- | Get a random piece type
randomPieceType :: StdGen -> (PieceType, StdGen)
randomPieceType rng =
    let (idx, rng') = uniformR (fromEnum (minBound :: PieceType), fromEnum (maxBound :: PieceType)) rng
    in (toEnum idx, rng')

-- | Handle a game event, returning new state
handleEvent :: Event -> GameState -> GameState
handleEvent event gs
    | gsGameOver gs = gs
    | otherwise = case event of
        MoveLeft  -> movePiece (-1, 0) gs
        MoveRight -> movePiece (1, 0) gs
        MoveDown  -> movePiece (0, 1) gs
        Rotate    -> rotatePiece gs
        HardDrop  -> hardDrop gs
        Tick      -> tick gs
        Quit      -> gs { gsGameOver = True }

-- | Get absolute board positions for the current piece
getPieceBlocks :: Piece -> [Position]
getPieceBlocks piece =
    let (px, py) = piecePosition piece
        offsets = pieceBlocks (pieceType piece) (pieceRotation piece)
    in [(px + dx, py + dy) | (dx, dy) <- offsets]

-- | Check if a piece collides with board boundaries or filled cells
checkCollision :: Piece -> Board -> Bool
checkCollision piece board =
    any outOfBounds blocks || any overlapsCell blocks
  where
    blocks = getPieceBlocks piece

    outOfBounds (x, y) =
        x < 0 || x >= boardWidth || y < 0 || y >= boardHeight

    overlapsCell (x, y)
        | y < 0 = False  -- Allow piece above board
        | otherwise = (board ! y) ! x /= Empty

-- | Try to move the piece by delta; return unchanged state if collision
movePiece :: Position -> GameState -> GameState
movePiece (dx, dy) gs =
    let piece = gsCurrentPiece gs
        (px, py) = piecePosition piece
        newPiece = piece { piecePosition = (px + dx, py + dy) }
    in if checkCollision newPiece (gsBoard gs)
       then gs
       else gs { gsCurrentPiece = newPiece }

-- | Try to rotate the piece; return unchanged state if collision
rotatePiece :: GameState -> GameState
rotatePiece gs =
    let piece = gsCurrentPiece gs
        newRot = (pieceRotation piece + 1) `mod` 4
        newPiece = piece { pieceRotation = newRot }
    in if checkCollision newPiece (gsBoard gs)
       then gs  -- Simple: no wall kicks
       else gs { gsCurrentPiece = newPiece }

-- | Drop piece instantly to bottom
hardDrop :: GameState -> GameState
hardDrop gs =
    let dropped = dropPiece gs
    in lockAndSpawn dropped

-- | Move piece down until it collides
dropPiece :: GameState -> GameState
dropPiece = until cannotMoveDown (movePiece (0, 1))
  where cannotMoveDown gs = gsCurrentPiece (movePiece (0, 1) gs) == gsCurrentPiece gs

-- | Gravity tick: move down or lock piece
tick :: GameState -> GameState
tick gs =
    let moved = movePiece (0, 1) gs
    in if gsCurrentPiece moved == gsCurrentPiece gs
       then lockAndSpawn gs  -- Can't move down, lock it
       else moved

-- | Lock current piece into board and spawn new piece
lockAndSpawn :: GameState -> GameState
lockAndSpawn gs =
    let boardWithPiece = lockPiece gs
        (clearedBoard, linesCount) = clearLines boardWithPiece
        newScore = gsScore gs + calculateScore linesCount (gsLevel gs)
        newTotalLines = gsLinesCleared gs + linesCount
        newLevel = 1 + newTotalLines `div` 10  -- Level up every 10 lines
        (nextType, newRng) = randomPieceType (gsRng gs)
        newPiece = Piece (gsNextPiece gs) 0 spawnPosition
        newState = gs
            { gsBoard = clearedBoard
            , gsCurrentPiece = newPiece
            , gsNextPiece = nextType
            , gsScore = newScore
            , gsLinesCleared = newTotalLines
            , gsLevel = newLevel
            , gsRng = newRng
            }
    in if checkCollision newPiece clearedBoard
       then newState { gsGameOver = True }  -- Can't spawn = game over
       else newState

-- | Bake current piece into the board
lockPiece :: GameState -> Board
lockPiece gs =
    let piece = gsCurrentPiece gs
        color = pieceColor (pieceType piece)
        blocks = getPieceBlocks piece
    in foldr (setCell color) (gsBoard gs) blocks
  where
    setCell :: Color -> Position -> Board -> Board
    setCell color (x, y) board
        | y < 0 || y >= boardHeight = board  -- Ignore blocks above board
        | otherwise =
            let row = board ! y
                newRow = row // [(x, Filled color)]
            in board // [(y, newRow)]

-- | Clear completed lines, return new board and count of lines cleared
clearLines :: Board -> (Board, Int)
clearLines board =
    let remaining = V.filter (not . isFullRow) board
        cleared = boardHeight - V.length remaining
        newRows = V.replicate cleared (V.replicate boardWidth Empty)
    in (newRows V.++ remaining, cleared)
  where
    isFullRow :: Vector Cell -> Bool
    isFullRow row = V.all (/= Empty) row

-- | Calculate score for clearing lines (original Nintendo scoring with level multiplier)
calculateScore :: Int -> Int -> Int
calculateScore lines level = baseScore * level
  where
    baseScore = case lines of
        0 -> 0
        1 -> 40
        2 -> 100
        3 -> 300
        4 -> 1200  -- Tetris!
        _ -> 1200  -- Shouldn't happen

-- | Get tick interval in milliseconds based on level
levelSpeed :: Int -> Int
levelSpeed level = max 50 (500 - (level - 1) * 50)
