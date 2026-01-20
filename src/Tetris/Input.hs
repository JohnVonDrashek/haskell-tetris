module Tetris.Input
    ( sdlEventToGameEvent
    , isQuitEvent
    ) where

import qualified SDL

import Tetris.Types

-- | Convert an SDL event to a game event (if applicable)
-- Takes game mode to provide context-aware key mapping
sdlEventToGameEvent :: GameMode -> SDL.Event -> Maybe Event
sdlEventToGameEvent mode event = case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent
        | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
            keyToEvent mode (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent))
    SDL.QuitEvent -> Just Quit
    _ -> Nothing

-- | Map key codes to game events based on current game mode
keyToEvent :: GameMode -> SDL.Keycode -> Maybe Event
keyToEvent mode keycode = case keycode of
    SDL.KeycodeLeft   -> Just MoveLeft
    SDL.KeycodeRight  -> Just MoveRight
    SDL.KeycodeDown   -> Just $ case mode of
        Playing -> MoveDown
        _       -> MenuDown
    SDL.KeycodeUp     -> Just $ case mode of
        Playing -> Rotate
        _       -> MenuUp
    SDL.KeycodeReturn -> Just MenuSelect
    SDL.KeycodeSpace  -> Just HardDrop
    SDL.KeycodeEscape -> Just Quit
    SDL.KeycodeQ      -> Just Quit
    _                 -> Nothing

-- | Check if an SDL event is a quit event
isQuitEvent :: GameMode -> SDL.Event -> Bool
isQuitEvent mode = maybe False (== Quit) . sdlEventToGameEvent mode
