module Rules (rules) where

import           SudokuTypes (Coords)
import qualified Data.Set as S
import qualified Data.List as L
import           Data.Function (on)

-- | A rule is a function that given coordinates and a given value returns a list of coordinates where a list of values must be removed
type Rule = (Coords, Int) -> [(Coords, S.Set Int)]

validCoords :: Coords -> Bool
validCoords (x, y) = 1 <= x && x <= 9 && 1 <= y && y <= 9

combineRules :: Rule -> Rule -> Rule
combineRules r1 r2 xyv = L.map combine . L.groupBy ((==) `on` fst) . L.sort $ r
  where
    combine [] = error "combineRules: Empty list"
    combine ((coords, values):cvs) =
      (coords, L.foldl (\acc -> S.union acc . snd) values cvs)

    r = r1 xyv ++ r2 xyv

rowRule :: Rule
rowRule ((x, y), v) = [((x', y), S.singleton v) | x' <- [1 .. 9]]

columnRule :: Rule
columnRule ((x, y), v) = [((x, y'), S.singleton v) | y' <- [1 .. 9]]

squareRule :: Rule
squareRule ((x, y), v) = let (x0, y0) = (quot (x - 1) 3, quot (y - 1) 3)
                         in [((3 * x0 + dx, 3 * y0 + dy), S.singleton v)
                            | dx <- [1 .. 3]
                            , dy <- [1 .. 3]]

baseRules :: Rule
baseRules = rowRule `combineRules` columnRule `combineRules` squareRule

{- These next rules are derived from cracking the cryptic's video [The Miracle Sudoku](https://www.youtube.com/watch?v=yKf9aUIxdb4) -}
knightRule :: Rule
knightRule ((x, y), v) = filter (validCoords . fst)
  $ [((x + dx, y + dy), S.singleton v) | dx <- [-1, 1], dy <- [-2, 2]]
  ++ [((x + dx, y + dy), S.singleton v) | dx <- [-2, 2], dy <- [-1, 1]]

kingRule :: Rule
kingRule ((x, y), v) = filter
  (validCoords . fst)
  [((x + dx, y + dy), S.singleton v) | dx <- [-1 .. 1], dy <- [-1 .. 1]]

orthogonalRule :: Rule
orthogonalRule ((x, y), v) = filter (validCoords . fst)
  $ [((x + dx, y), set) | dx <- [-1, 1]]
  ++ [((x, y + dy), set) | dy <- [-1, 1]]
  where
    set = s v

    s 1 = S.singleton 2
    s 9 = S.singleton 8
    s n = S.fromDistinctAscList [n - 1, n + 1]

miracleRules :: Rule
miracleRules = baseRules
  `combineRules` knightRule
  `combineRules` kingRule
  `combineRules` orthogonalRule

-- | Here you can specify the rules that you want to apply for the simulation. There is probably a better way of doing this but this is what I have found
rules :: Rule
rules = miracleRules
