# Haskell Tetris Implementation Plan

## Overview
A classic Tetris clone in Haskell using SDL2 for graphics. Focus on clean functional patterns, testable pure logic, and straightforward implementation.

## Key Design Decisions
- **Architecture**: Pure state machine (`GameState -> Event -> GameState`)
- **Graphics**: SDL2 via `haskell-game/sdl2` bindings
- **Build**: Cabal
- **Rotation**: Lookup tables (all 4 rotations predefined as data)
- **Scope**: Classic minimal - 7 tetrominos, rotation, line clearing, score, game over

## Project Structure
```
haskell-tetris/
├── haskell-tetris.cabal
├── app/
│   └── Main.hs           -- Entry point, game loop, SDL setup
├── src/
│   └── Tetris/
│       ├── Types.hs      -- Data types (GameState, Piece, Board, Event)
│       ├── Pieces.hs     -- Tetromino lookup tables (shapes, colors, rotations)
│       ├── Logic.hs      -- Pure game logic (move, rotate, tick, collision, line clear)
│       ├── Render.hs     -- SDL2 rendering (GameState -> IO ())
│       └── Input.hs      -- SDL event -> game Event translation
└── test/
    └── LogicSpec.hs      -- Tests for pure game logic
```

## Implementation Steps

### Step 1: Project Setup
- Initialize cabal project with `cabal init`
- Add dependencies: `sdl2`, `vector`, `random`
- Configure build with library + executable

### Step 2: Core Types (`src/Tetris/Types.hs`)
- `Board` - 10x20 grid using `Vector (Vector Cell)`
- `Cell` - `Empty` or `Filled Color`
- `PieceType` - `I | O | T | S | Z | J | L`
- `Piece` - type, rotation index, position, color
- `GameState` - board, current piece, next piece, score, game over flag, RNG
- `Event` - `MoveLeft | MoveRight | MoveDown | Rotate | HardDrop | Tick`

### Step 3: Piece Definitions (`src/Tetris/Pieces.hs`)
- Define all 7 tetrominos with their 4 rotation states as coordinate lists
- Each rotation is explicit data, no computation
- Include color mapping per piece type

### Step 4: Game Logic (`src/Tetris/Logic.hs`)
Pure functions:
- `initialState :: StdGen -> GameState` - create starting state
- `handleEvent :: Event -> GameState -> GameState` - main state transition
- `movePiece :: (Int, Int) -> GameState -> GameState` - move with collision check
- `rotatePiece :: GameState -> GameState` - cycle rotation index
- `tick :: GameState -> GameState` - gravity, lock piece, spawn new
- `checkCollision :: Piece -> Board -> Bool` - bounds + overlap check
- `lockPiece :: GameState -> GameState` - bake current piece into board
- `clearLines :: Board -> (Board, Int)` - remove full rows, return count
- `spawnPiece :: GameState -> GameState` - place next piece, check game over
- `calculateScore :: Int -> Int` - lines cleared to points

### Step 5: Input Handling (`src/Tetris/Input.hs`)
- `sdlEventToGameEvent :: SDL.Event -> Maybe Event`
- Map arrow keys, space (hard drop), up (rotate), escape (quit)

### Step 6: Rendering (`src/Tetris/Render.hs`)
- `render :: SDL.Renderer -> GameState -> IO ()`
- Draw board grid (filled cells as colored rectangles)
- Draw current piece
- Draw score text (SDL2_ttf or simple block-based numbers)
- Fixed cell size (e.g., 30x30 pixels)

### Step 7: Main Game Loop (`app/Main.hs`)
```
1. Initialize SDL, create window/renderer
2. Create initial GameState with random seed
3. Loop:
   a. Poll SDL events -> translate to game Events
   b. Apply gravity tick at fixed interval
   c. Fold events through handleEvent
   d. Render current state
   e. Check gameOver flag for exit
4. Cleanup SDL
```

### Step 8: Testing (`test/LogicSpec.hs`)
- Test collision detection with known board states
- Test line clearing logic
- Test piece rotation (lookup correctness)
- Test game over detection

## Dependencies
```yaml
build-depends:
  - base >= 4.14
  - sdl2 >= 2.5
  - vector >= 0.12
  - random >= 1.2
```

## Notes
- Board is 10 wide × 20 tall (standard Tetris)
- Pieces spawn at top center
- Gravity tick every ~500ms at start (can adjust)
- No wall kicks, ghost piece, or hold - keeping it simple
- Pure logic is fully testable without SDL
