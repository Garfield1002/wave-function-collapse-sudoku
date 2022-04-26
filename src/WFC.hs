{-
| Implementation of the wave function collapse algorithm to solve sudoku problems
-}
module WFC where

import           Data.Array ((//), (!))
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.List as L
import qualified Control.Monad as M
import qualified Data.Either as E
import qualified Data.Maybe as Maybe

type Coords = (Int, Int)

type SudokuCell = Either (S.Set Int) Int

type SudokuGrid = A.Array Coords SudokuCell

type Stack = [(Coords, Int)]

-- | Returns the grid cells sorted by ascending entropy
lowestEntropy :: SudokuGrid -> [(Coords, S.Set Int)]
lowestEntropy = L.sortOn foo
  . map (\((x, y), e) -> ((x, y), E.fromLeft S.empty e))
  . L.filter (E.isLeft . snd)
  . A.assocs
  where
    foo = S.size . snd

-- | Propagates the change at a given index
propagateAtIndex :: Stack -> [Coords] -> Int -> SudokuGrid -> Maybe SudokuGrid
propagateAtIndex _ [] _ grid = Just grid
propagateAtIndex stack ((x, y):xys) v grid = do
  (stack', grid') <- updateGrid
  propagateAtIndex stack' xys v grid'
  where
    updateGrid = case grid ! (x, y) of
      Left set
        -> (let set' = S.delete v set
            in case S.size set' of
                 0 -> Nothing
                 1 -> let v = S.elemAt 0 . E.fromLeft S.empty $ grid ! (x, y)
                      in Just
                           (((x, y), v):stack, grid // [((x, y), Left set')])
                 _ -> Just (stack, grid // [((x, y), Left set')]))
      Right n  -> Just (stack, grid)

-- | Collapses a given cell with a given value
collapse :: Stack -> SudokuGrid -> Maybe SudokuGrid
collapse [] grid = return grid
collapse (((x, y), v):xys) grid = do
  grid <- propagateColumn v $ grid // [((x, y), Right v)]
  grid <- propagateRow v grid
  propagateSquare v grid
  where
    propagateRow = propagateAtIndex xys [(x', y) | x' <- [1 .. 9]]

    propagateColumn = propagateAtIndex xys [(x, y') | y' <- [1 .. 9]]

    propagateSquare =
      let (x0, y0) = (quot (x - 1) 3, quot (y - 1) 3)
      in propagateAtIndex
           xys
           [(3 * x0 + dx, 3 * y0 + dy) | dx <- [1 .. 3], dy <- [1 .. 3]]

collapseRandom :: SudokuGrid -> Maybe [(SudokuGrid, Coords)]
collapseRandom grid = case l of
  [] -> Just [(grid, (0, 0))]
  ((coords, set):_) -> fmap
    ((grid, coords):)
    (case gridAttempts of
       [] -> Maybe.Nothing
       _  -> maybeHead
         $ Maybe.catMaybes [collapseRandom attempt | attempt <- gridAttempts])
    where
      gridAttempts =
        Maybe.catMaybes $ [collapse [(coords, v)] grid | v <- S.elems set]
  where
    l = lowestEntropy grid

    maybeHead [] = Nothing
    maybeHead (h:_) = Just h