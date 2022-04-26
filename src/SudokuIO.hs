module SudokuIO where

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Array as A
import           WFC (SudokuGrid)

filePath :: FilePath
filePath = "./sudoku.txt"

toIntList :: B.ByteString -> [Int]
toIntList bs = toInt <$> B.split ' ' bs
  where
    toInt s = int
      where
        Just (int, _) = B.readInt s

-- | read a txt sudoku and transforms it into a haskell object
-- sudoku are given as a grid where 0s represent empty cells
-- 0 0 3 0 2 0 6 0 0
-- 9 0 0 3 0 5 0 0 1
-- 0 0 1 8 0 6 4 0 0
-- 0 0 8 1 0 2 9 0 0
-- 7 0 0 0 0 0 0 0 8
-- 0 0 6 7 0 8 2 0 0
-- 0 0 2 6 0 9 5 0 0
-- 8 0 0 2 0 3 0 0 9
-- 0 0 5 0 1 0 3 0 0
readSudoku :: IO (A.Array (Int, Int) Int)
readSudoku = do
  lines <- B.lines <$> B.readFile filePath
  let n = length lines
  let grid = A.listArray ((1, 1), (n, n)) (concatMap toIntList lines)
  return grid

addNewLines :: (Eq t, Num t) => t -> [B.ByteString] -> B.ByteString
addNewLines _ [] = B.pack ""
addNewLines 9 (b:bs) = B.append (B.append b (B.pack "\n")) (addNewLines 1 bs)
addNewLines n (b:bs) = B.append b (addNewLines (n + 1) bs)

writeSudoku :: SudokuGrid -> IO ()
writeSudoku a = do
  let es = A.elems a
  let ls = map toBs es
  let s = addNewLines 1 . map (B.append space) $ ls
  B.appendFile "./sudoku_out.txt" s
  where
    toBs (Left _) = B.pack "?"
    toBs (Right i) = B.pack $ show i

    space = B.pack " "
