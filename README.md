# Wave Function Collapse - Sudoku

A visualization of the wave function collapse algorithm used here to solve a sudoku problem.

https://user-images.githubusercontent.com/53104608/165240320-c425d37c-1d68-4cb9-b79c-e21a0462eb65.mp4

It can also be used to solve more esoteric sudoku such as the [miracle sudoku by Mitchell Lee, solved by cracking the cryptic](https://www.youtube.com/watch?v=yKf9aUIxdb4)

In this version,

> Normal Sudoku rules apply. Any two cells separated by a knight's move or a king's move (in chess) cannot contain the same digit. Any two orthogonally adjacent cells cannot contain consecutive digits.

## 📚 Library

The animation was made using [Haskell reanimate](https://hackage.haskell.org/package/reanimate). With a lot of help from the [N-queen problem article](https://williamyaoh.com/posts/2020-05-31-reanimate-nqueens-tutorial.html).

## ⚙ Usage

You can specify and modify the rules for the game by editing [Rules.hs](https://github.com/Garfield1002/wave-function-collapse-sudoku/blob/master/src/Rules.hs).

The format for the input sudoku is a txt document with `0`s for empty squares and numbers for the given cells. For example:

```txt
0 0 0 0 0 0 0 3 0
0 9 0 6 8 0 0 0 5
0 4 2 0 1 0 6 0 0
0 2 0 8 0 0 0 0 0
0 0 1 0 0 0 0 0 0
4 0 3 0 0 0 0 0 9
0 0 0 0 0 0 0 2 0
6 8 0 1 0 0 4 0 0
0 0 0 0 0 4 0 0 3
```

I have found the best results by generating an mp4:

```bash
$ stack build
$ stack exec animate.exe -- render --format mp4 -o sudoku.mp4 -w 1280 -h 720 --fps 24
```

## 🩹 Limitations

This visualization however isn't perfect as it does not show off the stack or any backtracking.
