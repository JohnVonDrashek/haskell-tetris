# Main Menu Design

**Date:** 2026-01-19
**Status:** Approved

## Overview

Add a main menu system to Haskell Tetris with clean state management, keyboard navigation, and game flow that allows players to start games, retry after game over, and return to the main menu.

## Requirements

### Main Menu Screen
- Display game title "HASKELL TETRIS"
- Two menu options:
  - Start Game
  - Quit
- Arrow key navigation (Up/Down)
- Enter key to select
- Visual indicator (cursor) for selected item

### Game Over Screen
- Show dimmed game board in background
- Display "GAME OVER" overlay (existing)
- Show final score
- Two menu options:
  - Try Again (default selection)
  - Main Menu
- Same navigation as main menu

### Game Flow
1. Application starts → Main Menu
2. Select "Start Game" → Playing
3. Game Over condition → Game Over Screen
4. Select "Try Again" → Playing (new game)
5. Select "Main Menu" → Main Menu
6. Select "Quit" (main menu) → Exit application

## Architecture

### State Management

Following the existing pure functional architecture, extend the game state with a mode system:

**New Types (Types.hs):**

```haskell
data GameMode
    = MainMenu MenuState
    | Playing
    | GameOver MenuState
    deriving (Show, Eq)

data MenuState = MenuState
    { msSelectedIndex :: Int  -- 0-based index of highlighted menu item
    } deriving (Show, Eq)

data GameState = GameState
    { gsMode         :: GameMode  -- NEW: current screen/mode
    , gsBoard        :: Board
    , gsCurrentPiece :: Piece
    , gsNextPiece    :: PieceType
    , gsScore        :: Int
    , gsLinesCleared :: Int
    , gsGameOver     :: Bool
    , gsRng          :: StdGen
    } deriving (Show)
```

**Design Rationale:**
- Menu state is separate from game state but unified under one top-level type
- Each mode that needs menu navigation (MainMenu, GameOver) carries its own MenuState
- Playing mode has no menu state
- Preserves existing pure functional design - state transitions are pure functions

### Input Handling

**Event Extensions (Types.hs):**

```haskell
data Event
    = MoveLeft
    | MoveRight
    | MoveDown
    | Rotate
    | HardDrop
    | Tick
    | Quit
    | MenuUp      -- NEW: navigate menu up (maps from Arrow Up in menu context)
    | MenuDown    -- NEW: navigate menu down (maps from Arrow Down in menu context)
    | MenuSelect  -- NEW: select menu item (Enter key)
    deriving (Show, Eq)
```

**Input Mapping (Input.hs):**

SDL key events map to game events based on context:
- Arrow Up → `MenuUp` (in menus) or `Rotate` (in gameplay)
- Arrow Down → `MenuDown` (in menus) or `MoveDown` (in gameplay)
- Enter/Return → `MenuSelect`
- Escape/Q → `Quit` (works in all modes)

Context is determined by checking `gsMode` when converting SDL events.

**Event Routing (Logic.hs):**

```haskell
handleEvent :: Event -> GameState -> GameState
handleEvent event gs = case gsMode gs of
    MainMenu menuState -> handleMenuEvent event menuState gs
    Playing -> handleGameplayEvent event gs  -- existing logic
    GameOver menuState -> handleMenuEvent event menuState gs
```

Menu event handler logic:
- `MenuUp`: Decrement `msSelectedIndex` (with wrapping to bottom)
- `MenuDown`: Increment `msSelectedIndex` (with wrapping to top)
- `MenuSelect`: Execute action based on current mode and selected index

### State Transitions

**Transition Logic (Logic.hs):**

```haskell
handleMenuEvent :: Event -> MenuState -> GameState -> GameState
handleMenuEvent MenuUp ms gs =
    gs { gsMode = updateMode (ms { msSelectedIndex = prevIndex }) }

handleMenuEvent MenuDown ms gs =
    gs { gsMode = updateMode (ms { msSelectedIndex = nextIndex }) }

handleMenuEvent MenuSelect ms gs =
    case (gsMode gs, msSelectedIndex ms) of
        (MainMenu _, 0) -> startNewGame gs      -- "Start Game" selected
        (MainMenu _, 1) -> gs { gsGameOver = True }  -- "Quit" selected
        (GameOver _, 0) -> startNewGame gs      -- "Try Again" selected
        (GameOver _, 1) -> resetToMainMenu gs   -- "Main Menu" selected
        _ -> gs

startNewGame :: GameState -> GameState
startNewGame gs =
    let rng = gsRng gs  -- Preserve RNG for piece generation
    in (initialState rng) { gsMode = Playing }

resetToMainMenu :: GameState -> GameState
resetToMainMenu gs = gs { gsMode = MainMenu (MenuState 0) }
```

**State Diagram:**

```
MainMenu ──[Select "Start Game"]──> Playing
    ↑                                   │
    │                                   │
    │                            [Game Over condition]
    │                                   │
    │                                   ↓
    └──[Select "Main Menu"]──── GameOver
                                   │
                                   └──[Select "Try Again"]──> Playing
```

**Game Over Transition:**

When a game over condition occurs during gameplay, transition to GameOver mode:

```haskell
-- In handleEvent or game tick logic
if gameOverCondition
    then gs { gsMode = GameOver (MenuState 0), gsGameOver = True }
    else gs
```

Note: `gsGameOver = True` is still set for backward compatibility with game loop quit logic.

**Initial State (Main.hs):**

```haskell
initialState :: StdGen -> GameState
initialState rng = GameState
    { gsMode = MainMenu (MenuState 0)  -- Start at main menu, first item selected
    , gsBoard = emptyBoard
    , gsCurrentPiece = initialPiece
    , gsNextPiece = nextPiece
    , gsScore = 0
    , gsLinesCleared = 0
    , gsGameOver = False
    , gsRng = rng'
    }
```

### Rendering

Reuse existing rendering primitives (`drawLabel`, `fillRect`, bitmap font system) with new menu-specific rendering functions.

**Main Menu Layout:**

```
┌─────────────────────────────┐
│                             │
│      HASKELL TETRIS         │  <- Title (centered, large text)
│                             │
│                             │
│      > Start Game           │  <- Selected (with ">" cursor)
│        Quit                 │  <- Unselected
│                             │
│                             │
└─────────────────────────────┘
```

**Game Over Layout:**

```
┌─────────────────────────────┐
│  [Dimmed game board]        │
│                             │
│      GAME OVER              │  <- Existing red box overlay
│                             │
│      Score: 12450           │
│                             │
│      > Try Again            │  <- Selected (default)
│        Main Menu            │
│                             │
└─────────────────────────────┘
```

**Rendering Functions (Render.hs):**

```haskell
render :: SDL.Renderer -> GameState -> IO ()
render renderer gs = case gsMode gs of
    MainMenu menuState -> renderMainMenu renderer menuState
    Playing -> renderGameplay renderer gs  -- existing rendering
    GameOver menuState -> renderGameOver renderer gs menuState

renderMainMenu :: SDL.Renderer -> MenuState -> IO ()
-- Draw dark background
-- Draw "HASKELL TETRIS" title using drawLabel (centered)
-- Draw menu items with selection cursor

renderGameOver :: SDL.Renderer -> GameState -> MenuState -> IO ()
-- Draw dimmed game board (existing renderGameplay with dimming)
-- Draw game over overlay (existing drawGameOver)
-- Draw final score
-- Draw menu items with selection cursor
```

**Menu Item Rendering:**

- Use existing `drawLabel` function for text
- Draw ">" cursor before selected item using `drawLabel`
- Optionally draw boxes around menu items using `fillRect` (similar to score boxes)
- Selected item can have different color or background highlight

## Implementation Notes

### Files to Modify

1. **src/Tetris/Types.hs**
   - Add `GameMode` and `MenuState` types
   - Add `MenuUp`, `MenuDown`, `MenuSelect` to `Event`
   - Add `gsMode` field to `GameState`

2. **src/Tetris/Logic.hs**
   - Add `handleMenuEvent` function
   - Modify `handleEvent` to route based on `gsMode`
   - Add `startNewGame` and `resetToMainMenu` helper functions
   - Update game over condition to set `gsMode = GameOver (MenuState 0)`

3. **src/Tetris/Input.hs**
   - Modify `sdlEventToGameEvent` to map Enter key to `MenuSelect`
   - Map arrow keys contextually based on game mode (passed as parameter)

4. **src/Tetris/Render.hs**
   - Add `renderMainMenu` function
   - Add `renderGameOver` function (extend existing)
   - Modify main `render` function to route based on `gsMode`
   - Add helper for drawing menu items with selection cursor

5. **app/Main.hs**
   - Modify `initialState` to start with `gsMode = MainMenu (MenuState 0)`
   - Update game loop quit condition to handle both Quit event and `gsGameOver`

### Backward Compatibility

- `gsGameOver` field remains for compatibility with existing game loop logic
- Existing gameplay rendering and logic are unchanged
- Menu system is additive - no breaking changes to core game logic

### Testing Strategy

1. Manual testing of menu navigation (up/down wrapping, selection)
2. Manual testing of all state transitions
3. Verify game restart resets score, board, and level correctly
4. Verify returning to main menu preserves menu state
5. Existing unit tests should continue to pass (they test pure game logic only)

## Future Enhancements (Out of Scope)

- Settings menu (difficulty, starting level)
- High score persistence
- How to Play screen
- Sound effects for menu navigation
- Menu animations or transitions
