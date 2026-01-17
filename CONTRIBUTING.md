# Contributing to Haskell Tetris

Thank you for your interest in contributing to Haskell Tetris! Contributions are warmly welcomed, whether you're fixing a bug, adding a feature, improving documentation, or suggesting ideas.

## Our Promise

I respond to every pull request and issue. Your time and effort are valued, and you'll always hear back from me.

## What We Accept

- **Bug fixes**: Obvious accepts. If something is broken, let's fix it.
- **New features**: Welcome! If you have an idea that would make this project better, I'd love to see it.
- **Documentation improvements**: Always appreciated.
- **Test coverage**: More tests are always welcome.

## Getting Started

### Prerequisites

1. **GHC 9.4+** and **Cabal** - Install via [GHCup](https://www.haskell.org/ghcup/):
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ghcup install ghc 9.4
   ghcup set ghc 9.4
   ```

2. **SDL2 library** - Install via your system package manager:
   ```bash
   # macOS
   brew install sdl2

   # Ubuntu/Debian
   sudo apt-get install libsdl2-dev

   # Fedora
   sudo dnf install SDL2-devel
   ```

### Development Workflow

```bash
# Clone the repository
git clone https://github.com/JohnVonDrashek/haskell-tetris.git
cd haskell-tetris

# Build the project
cabal build

# Run the game
cabal run

# Run tests
cabal test
```

## Coding Standards

### Type Signatures

Always use explicit type signatures for top-level definitions:

```haskell
-- Good
rotatePiece :: Piece -> Piece
rotatePiece piece = ...

-- Avoid
rotatePiece piece = ...
```

### Pure Functions

Prefer pure functions over IO when possible. Keep side effects at the edges of your program:

```haskell
-- Prefer this
calculateScore :: Int -> Int -> Int
calculateScore linesCleared level = ...

-- Over embedding IO unnecessarily
```

### Naming Conventions

- **Functions and variables**: `camelCase`
- **Types and type constructors**: `PascalCase`
- **Modules**: `PascalCase`

```haskell
data GameState = GameState
  { currentPiece :: Piece
  , gameBoard :: Board
  , playerScore :: Int
  }

updateGameState :: GameState -> Input -> GameState
```

### Documentation

Add Haddock documentation to all exported functions:

```haskell
-- | Rotates a tetromino piece 90 degrees clockwise.
-- Returns the rotated piece with updated block positions.
rotatePiece :: Piece -> Piece
rotatePiece = ...

-- | Checks if the given position is valid on the board.
-- A position is valid if it's within bounds and not occupied.
isValidPosition
  :: Board   -- ^ The game board
  -> Piece   -- ^ The piece to check
  -> Bool    -- ^ True if the position is valid
isValidPosition board piece = ...
```

### Formatting

Follow standard Haskell formatting conventions:
- Use 2 or 4 space indentation (be consistent)
- Keep lines under 80-100 characters when reasonable
- Use explicit import lists or qualified imports

## Submitting Changes

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Ensure tests pass (`cabal test`)
5. Commit your changes with a clear message
6. Push to your fork
7. Open a Pull Request

## Questions or Ideas?

Feel free to open an issue for discussion, or reach out directly:

**Email**: johnvondrashek@gmail.com

## Code of Conduct

This project follows the [Rule of St. Benedict Code of Conduct](https://opensource.saintaardvarkthecarpeted.com/rule-of-st-benedict-code-of-conduct/). In short: be kind, be patient, listen well, and work together in peace.

---

Thank you for contributing!
