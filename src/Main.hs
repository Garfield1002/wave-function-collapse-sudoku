module Main (main) where

import           Reanimate
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Either as E
import qualified Data.Array as A
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import           WFC
import           SudokuTypes
import           SudokuIO
import           Codec.Picture (PixelRGBA8(..))
import           Rules (rules)

smallGrid :: SVG
smallGrid = withStrokeWidth 0.05 . withStrokeColor "gray" . withFillOpacity 0
  $ mkRect 1 1

sudokuGrid :: SVG
sudokuGrid = center
  $ scale 2.5
  $ mkGroup
  $ fmap smallGrid [0 .. 80] ++ fmap mediumGrid [0 .. 8]
  where
    smallGrid :: Int -> SVG
    smallGrid tileNumber =
      translate (fromIntegral (x - 1) / 3) (fromIntegral (y - 1) / 3) base
      where
        (y, x) = tileNumber `divMod` 9

        base = center
          . withStrokeWidth 0.005
          . withStrokeColor "gray"
          . withFillOpacity 0
          $ mkRect (1 / 3) (1 / 3)

    mediumGrid :: Int -> SVG
    mediumGrid tileNumber = translate (fromIntegral x) (fromIntegral y) base
      where
        (y, x) = tileNumber `divMod` 3

        base = center
          . withStrokeWidth 0.02
          . withStrokeColor "gray"
          . withFillOpacity 0
          $ mkRect 1 1

drawSet :: Coords -> S.Set Int -> SVG
drawSet (x, y) = withStrokeWidth 0
  . withFillColor "gray"
  . translate (-5 / 3 + fromIntegral x / 3) (5 / 3 - fromIntegral y / 3)
  . mkGroup
  . fmap numbers
  . S.elems
  where
    numbers :: Int -> SVG
    numbers n =
      let (dy, dx) = (n - 1) `divMod` 3
      in scale 0.9
         $ translate (fromIntegral (dx - 1) / 9) (fromIntegral (-dy + 1) / 9)
         $ center
         $ scale 0.075
         $ latex (T.pack $ show n)

drawNum :: Coords -> Int -> SVG
drawNum (x, y) n = withStrokeWidth 0
  . withFillColor "gray"
  . translate (-5 / 3 + fromIntegral x / 3) (5 / 3 - fromIntegral y / 3)
  . center
  . scale 0.3
  $ latex (T.pack $ show n)

drawSudokuCell :: Coords -> SudokuCell -> SVG
drawSudokuCell (x, y) (E.Left set) = drawSet (y, x) set
drawSudokuCell (x, y) (E.Right n) = drawNum (y, x) n

drawSudoku :: SudokuGrid -> SVG
drawSudoku sudoku = scale 2.5 . mkGroup . map (uncurry drawSudokuCell)
  $ A.assocs sudoku

highlight :: Coords -> Animation
highlight (y, x) = signalA (bellS 2)
  $ mkAnimation 0.25 (\t -> withFillOpacity (0.3 * t) highlightSVG)
  where
    highlightSVG = scale 2.5 . withFillColor "white" . mkGroup
      $ [translate (fromIntegral x' / 3 - 5 / 3) (5 / 3 - fromIntegral y' / 3)
          $ mkRect (1 / 3) (1 / 3)
        | ((x', y'), _) <- rules ((x, y), 1)]

    -- highlightLine = mkGroup
    --   [translate (fromIntegral (dx - 1)) (5 / 3 - fromIntegral y / 3)
    --     $ mkRect 1
    --     $ 1 / 3
    --   | dx <- [0 .. 2]
    --   , dx /= quot (x - 1) 3]
    -- highlightCol = mkGroup
    --   [translate (fromIntegral x / 3 - 5 / 3) (fromIntegral (-dy + 1))
    --     $ mkRect (1 / 3) 1
    --   | dy <- [0 .. 2]
    --   , dy /= quot (y - 1) 3]
    -- highlightSquare =
    --   let (x0, y0) = (quot (x - 1) 3, quot (y - 1) 3)
    --   in translate (fromIntegral x0 - 1) (1 - fromIntegral y0) $ mkRect 1 1
main :: IO ()
main = do
  base <- readSudoku
  let grid =
        A.listArray ((1, 1), (9, 9)) (repeat (Left (S.fromAscList [1 .. 9])))
      temp = L.filter ((/= 0) . snd) . A.assocs $ base
  let (a, history) = L.foldl propagate (grid, []) temp
  let solutionSteps = Maybe.fromJust (collapseRandom a)
  reanimate
    $ parA backgroundAnim
    $ foldl
      (\animation (grid', coords) -> seqA
         (mkAnimation 0.3 (const $ drawSudoku grid')
          `parDropA` highlight coords)
         animation)
      (mkAnimation 0 (const $ drawSudoku grid))
      (reverse (adjustCoords solutionSteps) ++ history)
  where
    propagate :: (SudokuGrid, [(SudokuGrid, Coords)])
              -> (Coords, Int)
              -> (SudokuGrid, [(SudokuGrid, Coords)])
    propagate (grid, history) (coords@(x, y), v) =
      (newGrid, (newGrid, coords):history)
      where
        newGrid = Maybe.fromJust $ collapse [((x, y), v)] grid

    backgroundAnim :: Animation
    backgroundAnim = parA
      (animate $ const $ mkBackgroundPixel $ PixelRGBA8 34 39 46 0xFF)
      (animate $ const sudokuGrid)

    adjustCoords :: [(SudokuGrid, Coords)] -> [(SudokuGrid, Coords)]
    adjustCoords ((_, coords):e@(grid, _):rest) = (grid, coords)
      :adjustCoords (e:rest)
    adjustCoords _ = []
