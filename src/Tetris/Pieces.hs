module Tetris.Pieces
    ( -- * Piece data
      pieceBlocks
    , pieceColor
    , allPieceTypes
    , spawnPosition
    ) where

import Tetris.Types

-- | All piece types for random selection
allPieceTypes :: [PieceType]
allPieceTypes = [minBound .. maxBound]

-- | Starting position for new pieces (top-center of board)
spawnPosition :: Position
spawnPosition = (3, 0)

-- | Consolidated tetromino data: color and 4 rotations
data TetroData = TetroData
    { tetroColor  :: Color
    , tetroShapes :: [[Position]]  -- 4 rotations
    }

-- | Unified lookup table for all piece properties
tetroData :: PieceType -> TetroData
tetroData I = TetroData (Color 0   255 255) iBlocks  -- Cyan
tetroData O = TetroData (Color 255 255 0  ) oBlocks  -- Yellow
tetroData T = TetroData (Color 128 0   128) tBlocks  -- Purple
tetroData S = TetroData (Color 0   255 0  ) sBlocks  -- Green
tetroData Z = TetroData (Color 255 0   0  ) zBlocks  -- Red
tetroData J = TetroData (Color 0   0   255) jBlocks  -- Blue
tetroData L = TetroData (Color 255 165 0  ) lBlocks  -- Orange

-- | Get the color for each piece type
pieceColor :: PieceType -> Color
pieceColor = tetroColor . tetroData

-- | Get the block offsets for a piece type and rotation.
--   Returns list of (dx, dy) offsets from the anchor position.
--   Rotation is 0-3 for 0, 90, 180, 270 degrees clockwise.
pieceBlocks :: PieceType -> Rotation -> [Position]
pieceBlocks pt rot = tetroShapes (tetroData pt) !! (rot `mod` 4)

-- I-piece: ####
-- Rotations (4 states, but effectively 2 unique)
iBlocks :: [[Position]]
iBlocks =
    [ [(0,1), (1,1), (2,1), (3,1)]   -- Horizontal (rot 0)
    , [(2,0), (2,1), (2,2), (2,3)]   -- Vertical (rot 1)
    , [(0,2), (1,2), (2,2), (3,2)]   -- Horizontal (rot 2)
    , [(1,0), (1,1), (1,2), (1,3)]   -- Vertical (rot 3)
    ]

-- O-piece: ##
--          ##
-- All rotations are identical
oBlocks :: [[Position]]
oBlocks = replicate 4 [(0,0), (1,0), (0,1), (1,1)]

-- T-piece:  #
--          ###
tBlocks :: [[Position]]
tBlocks =
    [ [(1,0), (0,1), (1,1), (2,1)]   -- T up (rot 0)
    , [(1,0), (1,1), (2,1), (1,2)]   -- T right (rot 1)
    , [(0,1), (1,1), (2,1), (1,2)]   -- T down (rot 2)
    , [(1,0), (0,1), (1,1), (1,2)]   -- T left (rot 3)
    ]

-- S-piece:  ##
--          ##
sBlocks :: [[Position]]
sBlocks =
    [ [(1,0), (2,0), (0,1), (1,1)]   -- Horizontal (rot 0)
    , [(1,0), (1,1), (2,1), (2,2)]   -- Vertical (rot 1)
    , [(1,1), (2,1), (0,2), (1,2)]   -- Horizontal (rot 2)
    , [(0,0), (0,1), (1,1), (1,2)]   -- Vertical (rot 3)
    ]

-- Z-piece: ##
--           ##
zBlocks :: [[Position]]
zBlocks =
    [ [(0,0), (1,0), (1,1), (2,1)]   -- Horizontal (rot 0)
    , [(2,0), (1,1), (2,1), (1,2)]   -- Vertical (rot 1)
    , [(0,1), (1,1), (1,2), (2,2)]   -- Horizontal (rot 2)
    , [(1,0), (0,1), (1,1), (0,2)]   -- Vertical (rot 3)
    ]

-- J-piece: #
--          ###
jBlocks :: [[Position]]
jBlocks =
    [ [(0,0), (0,1), (1,1), (2,1)]   -- J up (rot 0)
    , [(1,0), (2,0), (1,1), (1,2)]   -- J right (rot 1)
    , [(0,1), (1,1), (2,1), (2,2)]   -- J down (rot 2)
    , [(1,0), (1,1), (0,2), (1,2)]   -- J left (rot 3)
    ]

-- L-piece:   #
--          ###
lBlocks :: [[Position]]
lBlocks =
    [ [(2,0), (0,1), (1,1), (2,1)]   -- L up (rot 0)
    , [(1,0), (1,1), (1,2), (2,2)]   -- L right (rot 1)
    , [(0,1), (1,1), (2,1), (0,2)]   -- L down (rot 2)
    , [(0,0), (1,0), (1,1), (1,2)]   -- L left (rot 3)
    ]
