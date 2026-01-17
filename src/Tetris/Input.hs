module Tetris.Input
    ( sdlEventToGameEvent
    , isQuitEvent
    ) where

import qualified SDL

import Tetris.Types

-- | Convert an SDL event to a game event (if applicable)
sdlEventToGameEvent :: SDL.Event -> Maybe Event
sdlEventToGameEvent event = case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent
        | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
            keyToEvent (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent))
    SDL.QuitEvent -> Just Quit
    _ -> Nothing

-- | Map key codes to game events
keyToEvent :: SDL.Keycode -> Maybe Event
keyToEvent keycode = case keycode of
    SDL.KeycodeLeft   -> Just MoveLeft
    SDL.KeycodeRight  -> Just MoveRight
    SDL.KeycodeDown   -> Just MoveDown
    SDL.KeycodeUp     -> Just Rotate
    SDL.KeycodeSpace  -> Just HardDrop
    SDL.KeycodeEscape -> Just Quit
    SDL.KeycodeQ      -> Just Quit
    _                 -> Nothing

-- | Check if an SDL event is a quit event
isQuitEvent :: SDL.Event -> Bool
isQuitEvent event = case SDL.eventPayload event of
    SDL.QuitEvent -> True
    SDL.KeyboardEvent keyboardEvent
        | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
            let keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)
            in keycode == SDL.KeycodeEscape || keycode == SDL.KeycodeQ
    _ -> False
