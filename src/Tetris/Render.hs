{-# LANGUAGE OverloadedStrings #-}

module Tetris.Render
    ( render
    , cellSize
    , windowWidth
    , windowHeight
    ) where

import qualified SDL
import Control.Monad (when)
import qualified Data.Vector as V
import Data.Bits (testBit)
import Foreign.C.Types (CInt)

import Tetris.Types
import Tetris.Pieces
import Tetris.Logic (getPieceBlocks, calculateShadowPiece)

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
render renderer gs = case gsMode gs of
    MainMenu menuState -> do
        SDL.rendererDrawColor renderer SDL.$= SDL.V4 30 30 30 255
        SDL.clear renderer
        renderMainMenu renderer menuState
        SDL.present renderer

    Playing -> do
        SDL.rendererDrawColor renderer SDL.$= SDL.V4 30 30 30 255
        SDL.clear renderer
        drawBoardBackground renderer
        drawBoard renderer (gsBoard gs)
        drawShadowPiece renderer (gsCurrentPiece gs) (gsBoard gs)
        drawCurrentPiece renderer (gsCurrentPiece gs)
        drawNextPiecePreview renderer (gsNextPiece gs)
        drawScoreInfo renderer gs
        SDL.present renderer

    GameOver menuState -> do
        SDL.rendererDrawColor renderer SDL.$= SDL.V4 30 30 30 255
        SDL.clear renderer
        drawBoardBackground renderer
        drawBoard renderer (gsBoard gs)
        drawShadowPiece renderer (gsCurrentPiece gs) (gsBoard gs)
        drawCurrentPiece renderer (gsCurrentPiece gs)
        drawNextPiecePreview renderer (gsNextPiece gs)
        drawScoreInfo renderer gs
        drawGameOver renderer
        renderGameOverMenu renderer gs menuState
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
    mapM_ (drawGridLine renderer Vertical) [0..boardWidth]
    -- Horizontal lines
    mapM_ (drawGridLine renderer Horizontal) [0..boardHeight]

data LineDirection = Vertical | Horizontal

drawGridLine :: SDL.Renderer -> LineDirection -> Int -> IO ()
drawGridLine renderer dir idx = do
    let pos = boardPadding + fromIntegral idx * cellSize
        (p1, p2) = case dir of
            Vertical   -> (SDL.P (SDL.V2 pos boardPadding),
                           SDL.P (SDL.V2 pos (boardPadding + fromIntegral boardHeight * cellSize)))
            Horizontal -> (SDL.P (SDL.V2 boardPadding pos),
                           SDL.P (SDL.V2 (boardPadding + fromIntegral boardWidth * cellSize) pos))
    SDL.drawLine renderer p1 p2

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

-- | Dim a color to ~35% brightness for shadow effect
dimColor :: Color -> Color
dimColor (Color r g b) = Color (scale r) (scale g) (scale b)
  where
    scale c = floor (fromIntegral c * 0.35 :: Double)

-- | Draw the shadow (ghost) piece showing where current piece would land
drawShadowPiece :: SDL.Renderer -> Piece -> Board -> IO ()
drawShadowPiece renderer piece board = do
    let shadowPiece = calculateShadowPiece piece board
    -- Only draw if shadow is below current piece
    when (piecePosition shadowPiece /= piecePosition piece) $ do
        let color = pieceColor (pieceType piece)
            dimmedColor = dimColor color
            blocks = getPieceBlocks shadowPiece
        mapM_ (drawPieceBlock renderer dimmedColor) blocks

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
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 255
    drawLabel renderer (previewX + 30) (boardPadding + 28) "NEXT"

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
    let digits = map (\c -> fromEnum c - fromEnum '0') $ show n
        digitWidth = 12
    mapM_ (\(i, d) -> drawDigit renderer (x + fromIntegral i * digitWidth) y d)
          (zip [0..] digits)

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
drawLabel renderer x y label =
    mapM_ (\(i, c) -> drawLetter renderer (x + fromIntegral i * 10) y c) (zip [0..] label)

-- | Draw a single letter using a compact 5x7 pixel font
-- Each letter is encoded as 7 bytes, one per row, with bits representing pixels
drawLetter :: SDL.Renderer -> CInt -> CInt -> Char -> IO ()
drawLetter renderer x y char =
    mapM_ drawPixel [(px, py) | py <- [0..6], px <- [0..4], testBit (rows !! py) (4 - px)]
  where
    rows = letterData char
    drawPixel (px, py) =
        let rect = SDL.Rectangle (SDL.P (SDL.V2 (x + fromIntegral px * 2) (y + fromIntegral py * 2))) (SDL.V2 2 2)
        in SDL.fillRect renderer (Just rect)

-- | Compact 5x7 bitmap font data (7 bytes per character, MSB = leftmost pixel)
letterData :: Char -> [Int]
letterData 'A' = [0x0E, 0x11, 0x11, 0x1F, 0x11, 0x11, 0x11]  -- .###. #...# #...# ##### #...# #...# #...#
letterData 'C' = [0x0E, 0x11, 0x10, 0x10, 0x10, 0x11, 0x0E]  -- .###. #...# #.... #.... #.... #...# .###.
letterData 'E' = [0x1F, 0x10, 0x10, 0x1E, 0x10, 0x10, 0x1F]  -- ##### #.... #.... ####. #.... #.... #####
letterData 'G' = [0x0E, 0x11, 0x10, 0x17, 0x11, 0x11, 0x0F]  -- .###. #...# #.... #.### #...# #...# .####
letterData 'H' = [0x11, 0x11, 0x11, 0x1F, 0x11, 0x11, 0x11]  -- #...# #...# #...# ##### #...# #...# #...#
letterData 'I' = [0x1F, 0x04, 0x04, 0x04, 0x04, 0x04, 0x1F]  -- ##### ..#.. ..#.. ..#.. ..#.. ..#.. #####
letterData 'K' = [0x11, 0x12, 0x14, 0x18, 0x14, 0x12, 0x11]  -- #...# #..#. #.#.. ##... #.#.. #..#. #...#
letterData 'L' = [0x10, 0x10, 0x10, 0x10, 0x10, 0x10, 0x1F]  -- #.... #.... #.... #.... #.... #.... #####
letterData 'M' = [0x11, 0x1B, 0x15, 0x15, 0x11, 0x11, 0x11]  -- #...# ##.## #.#.# #.#.# #...# #...# #...#
letterData 'N' = [0x11, 0x19, 0x15, 0x15, 0x15, 0x13, 0x11]  -- #...# ##..# #.#.# #.#.# #.#.# #..## #...#
letterData 'O' = [0x0E, 0x11, 0x11, 0x11, 0x11, 0x11, 0x0E]  -- .###. #...# #...# #...# #...# #...# .###.
letterData 'Q' = [0x0E, 0x11, 0x11, 0x11, 0x15, 0x12, 0x0D]  -- .###. #...# #...# #...# #.#.# #..#. .##.#
letterData 'R' = [0x1E, 0x11, 0x11, 0x1E, 0x14, 0x12, 0x11]  -- ####. #...# #...# ####. #.#.. #..#. #...#
letterData 'S' = [0x0E, 0x11, 0x10, 0x0E, 0x01, 0x11, 0x0E]  -- .###. #...# #.... .###. ....# #...# .###.
letterData 'T' = [0x1F, 0x04, 0x04, 0x04, 0x04, 0x04, 0x04]  -- ##### ..#.. ..#.. ..#.. ..#.. ..#.. ..#..
letterData 'U' = [0x11, 0x11, 0x11, 0x11, 0x11, 0x11, 0x0E]  -- #...# #...# #...# #...# #...# #...# .###.
letterData 'V' = [0x11, 0x11, 0x11, 0x11, 0x11, 0x0A, 0x04]  -- #...# #...# #...# #...# #...# .#.#. ..#..
letterData 'X' = [0x11, 0x11, 0x0A, 0x04, 0x0A, 0x11, 0x11]  -- #...# #...# .#.#. ..#.. .#.#. #...# #...#
letterData 'Y' = [0x11, 0x11, 0x11, 0x0A, 0x04, 0x04, 0x04]  -- #...# #...# #...# .#.#. ..#.. ..#.. ..#..
letterData '>' = [0x10, 0x08, 0x04, 0x02, 0x04, 0x08, 0x10]  -- #.... .#... ..#.. ...#. ..#.. .#... #....
letterData _   = [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]  -- (blank/space)

-- | Draw game over overlay
drawGameOver :: SDL.Renderer -> IO ()
drawGameOver renderer = do
    -- Semi-transparent overlay over the game board
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 0 0 0 180
    let overlayRect = SDL.Rectangle
            (SDL.P (SDL.V2 boardPadding boardPadding))
            (SDL.V2 (fromIntegral boardWidth * cellSize) (fromIntegral boardHeight * cellSize))
    SDL.fillRect renderer (Just overlayRect)
-- | Render the main menu screen
renderMainMenu :: SDL.Renderer -> MenuState -> IO ()
renderMainMenu renderer menuState = do
    let centerX = windowWidth `div` 2
        titleY = 100
        menuY = 280

    -- Draw title "HASKELL TETRIS"
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 100 180 255 255
    drawLabel renderer (centerX - 65) titleY "HASKELL"
    drawLabel renderer (centerX - 35) (titleY + 30) "TETRIS"

    -- Draw menu items
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 200 200 200 255
    drawMenuItem renderer (centerX - 60) menuY "START GAME" (msSelectedIndex menuState == 0)
    drawMenuItem renderer (centerX - 25) (menuY + 30) "QUIT" (msSelectedIndex menuState == 1)

-- | Render the game over menu overlay
renderGameOverMenu :: SDL.Renderer -> GameState -> MenuState -> IO ()
renderGameOverMenu renderer gs menuState = do
    let centerX = boardPadding + fromIntegral boardWidth * cellSize `div` 2
        menuY = boardPadding + fromIntegral boardHeight * cellSize `div` 2 + 40

    -- Draw score
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 255 255
    drawLabel renderer (centerX - 40) (menuY - 20) "SCORE"
    drawNumber renderer (centerX - 20) menuY (gsScore gs)

    -- Draw menu items
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 200 200 200 255
    drawMenuItem renderer (centerX - 55) (menuY + 40) "TRY AGAIN" (msSelectedIndex menuState == 0)
    drawMenuItem renderer (centerX - 50) (menuY + 70) "MAIN MENU" (msSelectedIndex menuState == 1)

-- | Draw a menu item with optional selection cursor
drawMenuItem :: SDL.Renderer -> CInt -> CInt -> String -> Bool -> IO ()
drawMenuItem renderer x y text selected = do
    when selected $ do
        -- Draw selection cursor ">"
        SDL.rendererDrawColor renderer SDL.$= SDL.V4 255 255 100 255
        drawLabel renderer (x - 20) y ">"
    SDL.rendererDrawColor renderer SDL.$= SDL.V4 200 200 200 255
    drawLabel renderer x y text
