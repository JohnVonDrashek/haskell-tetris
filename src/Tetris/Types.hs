{-# LANGUAGE DeriveGeneric #-}

module Tetris.Types
    ( -- * Board types
      Cell(..)
    , Board
    , boardWidth
    , boardHeight
      -- * Piece types
    , PieceType(..)
    , Piece(..)
    , Position
    , Rotation
      -- * Game state
    , GameState(..)
    , gsLevel
    , Event(..)
      -- * Colors
    , Color(..)
    ) where

import Data.Vector (Vector)
import System.Random (StdGen)

-- | Board dimensions (standard Tetris)
boardWidth :: Int
boardWidth = 10

boardHeight :: Int
boardHeight = 20

-- | RGB color representation
data Color = Color !Word !Word !Word
    deriving (Show, Eq)

-- | A cell on the board is either empty or filled with a color
data Cell
    = Empty
    | Filled !Color
    deriving (Show, Eq)

-- | The board is a 2D grid of cells (row-major: outer vector is rows)
type Board = Vector (Vector Cell)

-- | The seven standard tetromino types
data PieceType
    = I  -- Cyan, 4-long
    | O  -- Yellow, 2x2 square
    | T  -- Purple, T-shape
    | S  -- Green, S-shape
    | Z  -- Red, Z-shape
    | J  -- Blue, J-shape
    | L  -- Orange, L-shape
    deriving (Show, Eq, Enum, Bounded)

-- | Position on the board (x, y) where (0,0) is top-left
type Position = (Int, Int)

-- | Rotation state: 0, 1, 2, or 3 (90-degree increments)
type Rotation = Int

-- | A piece in play
data Piece = Piece
    { pieceType     :: !PieceType
    , pieceRotation :: !Rotation
    , piecePosition :: !Position  -- Position of the piece's anchor point
    } deriving (Show, Eq)

-- | Game events (input + timer)
data Event
    = MoveLeft
    | MoveRight
    | MoveDown
    | Rotate
    | HardDrop
    | Tick
    | Quit
    deriving (Show, Eq)

-- | Complete game state
data GameState = GameState
    { gsBoard        :: !Board
    , gsCurrentPiece :: !Piece
    , gsNextPiece    :: !PieceType
    , gsScore        :: !Int
    , gsLinesCleared :: !Int
    , gsGameOver     :: !Bool
    , gsRng          :: !StdGen
    } deriving (Show)

-- | Derived level from lines cleared (level up every 10 lines)
gsLevel :: GameState -> Int
gsLevel gs = 1 + gsLinesCleared gs `div` 10
