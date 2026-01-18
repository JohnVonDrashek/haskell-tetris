{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified SDL
import Control.Monad (unless)
import Data.Maybe (mapMaybe)
import Data.Word (Word32)
import System.Random (initStdGen)

import Tetris.Types
import Tetris.Logic
import Tetris.Render
import Tetris.Input

main :: IO ()
main = do
    -- Initialize SDL
    SDL.initializeAll

    -- Create window and renderer
    window <- SDL.createWindow "Haskell Tetris"
        SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 windowWidth windowHeight }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    -- Initialize game state
    rng <- initStdGen
    let initialGameState = initialState rng

    -- Run game loop
    initialTicks <- SDL.ticks
    gameLoop renderer initialGameState initialTicks

    -- Cleanup
    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

-- | Main game loop
gameLoop :: SDL.Renderer -> GameState -> Word32 -> IO ()
gameLoop renderer gameState lastTickTime = do
    -- Get current time
    currentTime <- SDL.ticks

    -- Process SDL events
    events <- SDL.pollEvents
    let gameEvents = mapMaybe sdlEventToGameEvent events
        shouldQuit = any isQuitEvent events || gsGameOver gameState

    -- Check if it's time for a gravity tick
    let tickInterval = fromIntegral (levelSpeed (gsLevel gameState))
        timeSinceLastTick = currentTime - lastTickTime
        (shouldTick, newTickTime) =
            if timeSinceLastTick >= tickInterval
            then (True, currentTime)
            else (False, lastTickTime)

    -- Apply events to game state
    let eventsWithTick = if shouldTick then gameEvents ++ [Tick] else gameEvents
        newState = foldl (flip handleEvent) gameState eventsWithTick

    -- Render
    render renderer newState

    -- Small delay to prevent CPU spinning
    SDL.delay 16  -- ~60 FPS max

    -- Continue loop unless quitting
    unless shouldQuit $
        gameLoop renderer newState newTickTime
