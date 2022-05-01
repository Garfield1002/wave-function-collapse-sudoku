module SudokuTypes where

import qualified Data.Array as A
import qualified Data.Set as S

type Coords = (Int, Int)

type SudokuCell = Either (S.Set Int) Int

type SudokuGrid = A.Array Coords SudokuCell