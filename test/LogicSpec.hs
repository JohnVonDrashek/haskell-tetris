module LogicSpec (main) where

import Test.Hspec
import qualified Data.Vector as V
import System.Random (mkStdGen)

import Tetris.Types
import Tetris.Pieces
import Tetris.Logic

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "emptyBoard" $ do
        it "has correct dimensions" $ do
            V.length emptyBoard `shouldBe` boardHeight
            V.length (V.head emptyBoard) `shouldBe` boardWidth

        it "is all empty cells" $ do
            let allEmpty = V.all (V.all (== Empty)) emptyBoard
            allEmpty `shouldBe` True

    describe "initialState" $ do
        it "starts with empty board" $ do
            let gs = initialState (mkStdGen 42)
            V.all (V.all (== Empty)) (gsBoard gs) `shouldBe` True

        it "starts at level 1" $ do
            let gs = initialState (mkStdGen 42)
            gsLevel gs `shouldBe` 1

        it "starts with score 0" $ do
            let gs = initialState (mkStdGen 42)
            gsScore gs `shouldBe` 0

        it "starts not game over" $ do
            let gs = initialState (mkStdGen 42)
            gsGameOver gs `shouldBe` False

    describe "pieceBlocks" $ do
        it "I piece has 4 blocks" $ do
            length (pieceBlocks I 0) `shouldBe` 4

        it "O piece has 4 blocks" $ do
            length (pieceBlocks O 0) `shouldBe` 4

        it "all pieces have 4 blocks in all rotations" $ do
            let allFour = all (\pt -> all (\r -> length (pieceBlocks pt r) == 4) [0..3])
                              allPieceTypes
            allFour `shouldBe` True

        it "O piece is same in all rotations" $ do
            let rotations = map (pieceBlocks O) [0..3]
            all (== head rotations) rotations `shouldBe` True

    describe "checkCollision" $ do
        it "detects left boundary collision" $ do
            let piece = Piece I 0 (-2, 5)
            checkCollision piece emptyBoard `shouldBe` True

        it "detects right boundary collision" $ do
            let piece = Piece I 0 (8, 5)
            checkCollision piece emptyBoard `shouldBe` True

        it "detects bottom boundary collision" $ do
            let piece = Piece I 1 (5, 18)
            checkCollision piece emptyBoard `shouldBe` True

        it "no collision in valid position" $ do
            let piece = Piece T 0 (4, 10)
            checkCollision piece emptyBoard `shouldBe` False

        it "detects collision with filled cell" $ do
            let piece = Piece O 0 (4, 5)
                board = setBoardCell emptyBoard 4 6 (Filled (Color 255 0 0))
            checkCollision piece board `shouldBe` True

    describe "clearLines" $ do
        it "clears a full row" $ do
            let fullRow = V.replicate boardWidth (Filled (Color 255 0 0))
                board = emptyBoard V.// [(19, fullRow)]
                (newBoard, count) = clearLines board
            count `shouldBe` 1
            V.all (V.all (== Empty)) newBoard `shouldBe` True

        it "clears multiple full rows" $ do
            let fullRow = V.replicate boardWidth (Filled (Color 255 0 0))
                board = emptyBoard V.// [(18, fullRow), (19, fullRow)]
                (_, count) = clearLines board
            count `shouldBe` 2

        it "preserves partial rows" $ do
            let partialRow = V.replicate (boardWidth - 1) (Filled (Color 255 0 0))
                              V.++ V.singleton Empty
                board = emptyBoard V.// [(19, partialRow)]
                (_, count) = clearLines board
            count `shouldBe` 0

    describe "calculateScore" $ do
        it "scores 0 for 0 lines" $
            calculateScore 0 1 `shouldBe` 0

        it "scores 40 for 1 line at level 1" $
            calculateScore 1 1 `shouldBe` 40

        it "scores 100 for 2 lines at level 1" $
            calculateScore 2 1 `shouldBe` 100

        it "scores 300 for 3 lines at level 1" $
            calculateScore 3 1 `shouldBe` 300

        it "scores 1200 for 4 lines (Tetris) at level 1" $
            calculateScore 4 1 `shouldBe` 1200

        it "multiplies by level" $
            calculateScore 1 5 `shouldBe` 200

    describe "movePiece" $ do
        it "moves piece left when valid" $ do
            let gs = initialState (mkStdGen 42)
                movedGs = movePiece (-1, 0) gs
                (px, _) = piecePosition (gsCurrentPiece gs)
                (px', _) = piecePosition (gsCurrentPiece movedGs)
            px' `shouldBe` px - 1

        it "moves piece right when valid" $ do
            let gs = initialState (mkStdGen 42)
                movedGs = movePiece (1, 0) gs
                (px, _) = piecePosition (gsCurrentPiece gs)
                (px', _) = piecePosition (gsCurrentPiece movedGs)
            px' `shouldBe` px + 1

        it "moves piece down when valid" $ do
            let gs = initialState (mkStdGen 42)
                movedGs = movePiece (0, 1) gs
                (_, py) = piecePosition (gsCurrentPiece gs)
                (_, py') = piecePosition (gsCurrentPiece movedGs)
            py' `shouldBe` py + 1

    describe "rotatePiece" $ do
        it "cycles rotation 0 -> 1 -> 2 -> 3 -> 0" $ do
            let gs = initialState (mkStdGen 42)
                r0 = pieceRotation (gsCurrentPiece gs)
                gs1 = rotatePiece gs
                r1 = pieceRotation (gsCurrentPiece gs1)
            r1 `shouldBe` (r0 + 1) `mod` 4

    describe "levelSpeed" $ do
        it "starts at 500ms for level 1" $
            levelSpeed 1 `shouldBe` 500

        it "decreases by 50ms per level" $
            levelSpeed 2 `shouldBe` 450

        it "has minimum of 50ms" $
            levelSpeed 100 `shouldBe` 50

-- Helper to set a cell on the board
setBoardCell :: Board -> Int -> Int -> Cell -> Board
setBoardCell board x y cell =
    let row = board V.! y
        newRow = row V.// [(x, cell)]
    in board V.// [(y, newRow)]
