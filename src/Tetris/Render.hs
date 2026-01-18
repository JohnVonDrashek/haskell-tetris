{-# LANGUAGE OverloadedStrings #-}

module Tetris.Render
    ( render
    , cellSize
    , windowWidth
    , windowHeight
    ) where

import qualified SDL
import qualified Data.Vector as V
import Foreign.C.Types (CInt)

import Tetris.Types
import Tetris.Pieces
import Tetris.Logic (getPieceBlocks)

-- | Cell size in pixels
cellSize :: CInt
cellSize = 30

-- | Padding around the board
boardPadding :: CInt
boardPadding = 20

-- | Width of next piece preview area
previewWidth :: CInt
previewWidth = 150

-- | Convert board coordinates to screen pixel coordinates (with 1px inset for cell border)
boardToScreen :: Int -> Int -> (CInt, CInt)
boardToScreen x y =
    ( boardPadding + fromIntegral x * cellSize + 1
    , boardPadding + fromIntegral y * cellSize + 1
    )

-- | Window dimensions
windowWidth :: CInt
windowWidth = boardPadding + fromIntegral boardWidth * cellSize + boardPadding + previewWidth

windowHeight :: CInt
windowHeight = boardPadding + fromIntegral boardHeight * cellSize + boardPadding

-- | Render the entire game state
render :: SDL.Renderer -> GameState -> IO ()
render renderer gs = do
    -- Clear screen with dark background
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 30 30 30 255
    SDL.clear renderer

    -- Draw board background
    drawBoardBackground renderer

    -- Draw filled cells on the board
    drawBoard renderer (gsBoard gs)

    -- Draw current piece
    drawCurrentPiece renderer (gsCurrentPiece gs)

    -- Draw next piece preview
    drawNextPiecePreview renderer (gsNextPiece gs)

    -- Draw score and level
    drawScoreInfo renderer gs

    -- Draw game over overlay if needed
    when (gsGameOver gs) $ drawGameOver renderer

    SDL.present renderer

-- | Draw the board background grid
drawBoardBackground :: SDL.Renderer -> IO ()
drawBoardBackground renderer = do
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 40 40 40 255
    let rect = SDL.Rectangle
            (SDL.P (SDL.V2 boardPadding boardPadding))
            (SDL.V2 (fromIntegral boardWidth * cellSize) (fromIntegral boardHeight * cellSize))
    SDL.fillRect renderer (Just rect)

    -- Draw grid lines
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 50 50 50 255
    -- Vertical lines
    mapM_ (drawVerticalLine renderer) [0..boardWidth]
    -- Horizontal lines
    mapM_ (drawHorizontalLine renderer) [0..boardHeight]

drawVerticalLine :: SDL.Renderer -> Int -> IO ()
drawVerticalLine renderer x = do
    let x' = boardPadding + fromIntegral x * cellSize
    SDL.drawLine renderer
        (SDL.P (SDL.V2 x' boardPadding))
        (SDL.P (SDL.V2 x' (boardPadding + fromIntegral boardHeight * cellSize)))

drawHorizontalLine :: SDL.Renderer -> Int -> IO ()
drawHorizontalLine renderer y = do
    let y' = boardPadding + fromIntegral y * cellSize
    SDL.drawLine renderer
        (SDL.P (SDL.V2 boardPadding y'))
        (SDL.P (SDL.V2 (boardPadding + fromIntegral boardWidth * cellSize) y'))

-- | Draw filled cells on the board
drawBoard :: SDL.Renderer -> Board -> IO ()
drawBoard renderer board =
    V.imapM_ (drawRow renderer) board

drawRow :: SDL.Renderer -> Int -> V.Vector Cell -> IO ()
drawRow renderer y row =
    V.imapM_ (\x cell -> drawCell renderer x y cell) row

drawCell :: SDL.Renderer -> Int -> Int -> Cell -> IO ()
drawCell _ _ _ Empty = return ()
drawCell renderer x y (Filled (Color r g b)) = do
    let (screenX, screenY) = boardToScreen x y
        rect = SDL.Rectangle
            (SDL.P (SDL.V2 screenX screenY))
            (SDL.V2 (cellSize - 2) (cellSize - 2))
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
    SDL.fillRect renderer (Just rect)

-- | Draw the current falling piece
drawCurrentPiece :: SDL.Renderer -> Piece -> IO ()
drawCurrentPiece renderer piece = do
    let color = pieceColor (pieceType piece)
        blocks = getPieceBlocks piece
    mapM_ (drawPieceBlock renderer color) blocks

drawPieceBlock :: SDL.Renderer -> Color -> Position -> IO ()
drawPieceBlock renderer (Color r g b) (x, y)
    | y < 0 = return ()  -- Don't draw blocks above the board
    | otherwise = do
        let (screenX, screenY) = boardToScreen x y
            rect = SDL.Rectangle
                (SDL.P (SDL.V2 screenX screenY))
                (SDL.V2 (cellSize - 2) (cellSize - 2))
        SDL.rendererDrawColor renderer SDL.$= SDL.V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
        SDL.fillRect renderer (Just rect)

-- | Draw the next piece preview
drawNextPiecePreview :: SDL.Renderer -> PieceType -> IO ()
drawNextPiecePreview renderer nextType = do
    let previewX = boardPadding + fromIntegral boardWidth * cellSize + 30
        previewY = boardPadding + 60
        color = pieceColor nextType
        blocks = pieceBlocks nextType 0
        smallCell = cellSize - 6

    -- Draw "NEXT" label area
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 60 60 60 255
    let labelRect = SDL.Rectangle
            (SDL.P (SDL.V2 previewX (boardPadding + 20)))
            (SDL.V2 100 30)
    SDL.fillRect renderer (Just labelRect)

    -- Draw preview background
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 40 40 40 255
    let bgRect = SDL.Rectangle
            (SDL.P (SDL.V2 previewX previewY))
            (SDL.V2 (4 * smallCell + 10) (4 * smallCell + 10))
    SDL.fillRect renderer (Just bgRect)

    -- Draw preview blocks
    let Color r g b = color
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 (fromIntegral r) (fromIntegral g) (fromIntegral b) 255
    mapM_ (drawPreviewBlock previewX previewY smallCell) blocks
  where
    drawPreviewBlock px py size (dx, dy) = do
        let rect = SDL.Rectangle
                (SDL.P (SDL.V2 (px + 5 + fromIntegral dx * size) (py + 5 + fromIntegral dy * size)))
                (SDL.V2 (size - 2) (size - 2))
        SDL.fillRect renderer (Just rect)

-- | Draw score and level information
drawScoreInfo :: SDL.Renderer -> GameState -> IO ()
drawScoreInfo renderer gs = do
    let infoX = boardPadding + fromIntegral boardWidth * cellSize + 30
        labelHeight = 18
        boxHeight = 40
        spacing = 8
        scoreY = boardPadding + 180
        scoreLabelY = scoreY
        scoreBoxY = scoreY + labelHeight + 2
        levelY = scoreBoxY + boxHeight + spacing
        levelLabelY = levelY
        levelBoxY = levelY + labelHeight + 2
        linesY = levelBoxY + boxHeight + spacing
        linesLabelY = linesY
        linesBoxY = linesY + labelHeight + 2

    -- Draw labels
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 180 180 180 255
    drawLabel renderer infoX scoreLabelY "SCORE"
    drawLabel renderer infoX levelLabelY "LEVEL"
    drawLabel renderer infoX linesLabelY "LINES"

    -- Draw info boxes
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 60 60 60 255

    -- Score box
    let scoreRect = SDL.Rectangle (SDL.P (SDL.V2 infoX scoreBoxY)) (SDL.V2 100 boxHeight)
    SDL.fillRect renderer (Just scoreRect)

    -- Level box
    let levelRect = SDL.Rectangle (SDL.P (SDL.V2 infoX levelBoxY)) (SDL.V2 100 boxHeight)
    SDL.fillRect renderer (Just levelRect)

    -- Lines box
    let linesRect = SDL.Rectangle (SDL.P (SDL.V2 infoX linesBoxY)) (SDL.V2 100 boxHeight)
    SDL.fillRect renderer (Just linesRect)

    -- Draw numeric displays using simple rectangles
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 200 200 200 255
    drawNumber renderer (infoX + 10) (scoreBoxY + 20) (gsScore gs)
    drawNumber renderer (infoX + 10) (levelBoxY + 20) (gsLevel gs)
    drawNumber renderer (infoX + 10) (linesBoxY + 20) (gsLinesCleared gs)

-- | Draw a number using simple block segments (crude but works without fonts)
drawNumber :: SDL.Renderer -> CInt -> CInt -> Int -> IO ()
drawNumber renderer x y n = do
    let digits = if n == 0 then [0] else toDigits n
        digitWidth = 12
    mapM_ (\(i, d) -> drawDigit renderer (x + fromIntegral i * digitWidth) y d)
          (zip [0..] digits)
  where
    toDigits 0 = []
    toDigits num = toDigits (num `div` 10) ++ [num `mod` 10]

-- | Draw a single digit (very simple representation)
drawDigit :: SDL.Renderer -> CInt -> CInt -> Int -> IO ()
drawDigit renderer x y digit = do
    let segmentOn = digitSegments !! digit
        w = 8
        h = 3
    -- Seven-segment display layout:
    --  0
    -- 1 2
    --  3
    -- 4 5
    --  6
    when (segmentOn !! 0) $ fillSeg (x + 1) y w h           -- top
    when (segmentOn !! 1) $ fillSeg x (y + 1) h (w - 2)     -- upper-left
    when (segmentOn !! 2) $ fillSeg (x + w - h + 1) (y + 1) h (w - 2)  -- upper-right
    when (segmentOn !! 3) $ fillSeg (x + 1) (y + w - 1) w h  -- middle
    when (segmentOn !! 4) $ fillSeg x (y + w) h (w - 2)      -- lower-left
    when (segmentOn !! 5) $ fillSeg (x + w - h + 1) (y + w) h (w - 2)  -- lower-right
    when (segmentOn !! 6) $ fillSeg (x + 1) (y + 2 * w - 2) w h  -- bottom
  where
    fillSeg sx sy sw sh = do
        let rect = SDL.Rectangle (SDL.P (SDL.V2 sx sy)) (SDL.V2 sw sh)
        SDL.fillRect renderer (Just rect)

-- | Segment patterns for digits 0-9
digitSegments :: [[Bool]]
digitSegments =
    [ [True, True, True, False, True, True, True]      -- 0
    , [False, False, True, False, False, True, False]  -- 1
    , [True, False, True, True, True, False, True]     -- 2
    , [True, False, True, True, False, True, True]     -- 3
    , [False, True, True, True, False, True, False]    -- 4
    , [True, True, False, True, False, True, True]     -- 5
    , [True, True, False, True, True, True, True]      -- 6
    , [True, False, True, False, False, True, False]   -- 7
    , [True, True, True, True, True, True, True]       -- 8
    , [True, True, True, True, False, True, True]      -- 9
    ]

-- | Draw a text label using simple pixel letters
drawLabel :: SDL.Renderer -> CInt -> CInt -> String -> IO ()
drawLabel renderer x y label = do
    let letterWidth = 10
    mapM_ (\(i, c) -> drawLetter renderer (x + fromIntegral i * letterWidth) y c)
          (zip [0..] label)

-- | Draw a single letter using a simple 5x7 pixel font
drawLetter :: SDL.Renderer -> CInt -> CInt -> Char -> IO ()
drawLetter renderer x y char = do
    let pattern = letterPattern char
        pixelSize = 2
    mapM_ (drawPixel pixelSize) [(px, py) | py <- [0..6], px <- [0..4],
                                            pattern !! py !! px]
  where
    drawPixel size (px, py) = do
        let rect = SDL.Rectangle
                (SDL.P (SDL.V2 (x + fromIntegral px * size) (y + fromIntegral py * size)))
                (SDL.V2 size size)
        SDL.fillRect renderer (Just rect)

-- | 5x7 pixel patterns for letters (True = filled pixel)
letterPattern :: Char -> [[Bool]]
letterPattern 'S' =
    [ [False, True, True, True, False]
    , [True, False, False, False, True]
    , [True, False, False, False, False]
    , [False, True, True, True, False]
    , [False, False, False, False, True]
    , [True, False, False, False, True]
    , [False, True, True, True, False]
    ]
letterPattern 'C' =
    [ [False, True, True, True, False]
    , [True, False, False, False, True]
    , [True, False, False, False, False]
    , [True, False, False, False, False]
    , [True, False, False, False, False]
    , [True, False, False, False, True]
    , [False, True, True, True, False]
    ]
letterPattern 'O' =
    [ [False, True, True, True, False]
    , [True, False, False, False, True]
    , [True, False, False, False, True]
    , [True, False, False, False, True]
    , [True, False, False, False, True]
    , [True, False, False, False, True]
    , [False, True, True, True, False]
    ]
letterPattern 'R' =
    [ [True, True, True, True, False]
    , [True, False, False, False, True]
    , [True, False, False, False, True]
    , [True, True, True, True, False]
    , [True, False, True, False, False]
    , [True, False, False, True, False]
    , [True, False, False, False, True]
    ]
letterPattern 'E' =
    [ [True, True, True, True, True]
    , [True, False, False, False, False]
    , [True, False, False, False, False]
    , [True, True, True, True, False]
    , [True, False, False, False, False]
    , [True, False, False, False, False]
    , [True, True, True, True, True]
    ]
letterPattern 'L' =
    [ [True, False, False, False, False]
    , [True, False, False, False, False]
    , [True, False, False, False, False]
    , [True, False, False, False, False]
    , [True, False, False, False, False]
    , [True, False, False, False, False]
    , [True, True, True, True, True]
    ]
letterPattern 'V' =
    [ [True, False, False, False, True]
    , [True, False, False, False, True]
    , [True, False, False, False, True]
    , [True, False, False, False, True]
    , [True, False, False, False, True]
    , [False, True, False, True, False]
    , [False, False, True, False, False]
    ]
letterPattern 'I' =
    [ [True, True, True, True, True]
    , [False, False, True, False, False]
    , [False, False, True, False, False]
    , [False, False, True, False, False]
    , [False, False, True, False, False]
    , [False, False, True, False, False]
    , [True, True, True, True, True]
    ]
letterPattern 'N' =
    [ [True, False, False, False, True]
    , [True, True, False, False, True]
    , [True, False, True, False, True]
    , [True, False, True, False, True]
    , [True, False, True, False, True]
    , [True, False, False, True, True]
    , [True, False, False, False, True]
    ]
letterPattern _ =
    [ [False, False, False, False, False]
    , [False, False, False, False, False]
    , [False, False, False, False, False]
    , [False, False, False, False, False]
    , [False, False, False, False, False]
    , [False, False, False, False, False]
    , [False, False, False, False, False]
    ]

-- | Draw game over overlay
drawGameOver :: SDL.Renderer -> IO ()
drawGameOver renderer = do
    -- Semi-transparent overlay
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 180
    let overlayRect = SDL.Rectangle
            (SDL.P (SDL.V2 boardPadding boardPadding))
            (SDL.V2 (fromIntegral boardWidth * cellSize) (fromIntegral boardHeight * cellSize))
    SDL.fillRect renderer (Just overlayRect)

    -- "GAME OVER" text box
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 200 50 50 255
    let textBoxX = boardPadding + fromIntegral boardWidth * cellSize `div` 2 - 60
        textBoxY = boardPadding + fromIntegral boardHeight * cellSize `div` 2 - 20
        textRect = SDL.Rectangle (SDL.P (SDL.V2 textBoxX textBoxY)) (SDL.V2 120 40)
    SDL.fillRect renderer (Just textRect)

-- | Helper: when for IO
when :: Bool -> IO () -> IO ()
when True action = action
when False _     = return ()
