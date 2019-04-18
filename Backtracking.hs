module Backtracking where

import Utils
import System.Random (StdGen, randomR)
import qualified Data.Set as S


generate :: Int -> Int -> StdGen -> Grid
generate wide tall gen =
  backtracking grid (x,y) gen2 (S.singleton (x,y)) [(x,y)]
  where
    (max_x, max_y) = getBounds grid
    (x, gen1) = randomR (0, max_x) gen
    (y, gen2) = randomR (0, max_y) gen1
    grid = generateNewMaze wide tall

backtracking :: Grid -> Coord -> StdGen -> S.Set Coord -> [Coord] -> Grid
backtracking grid _  _   _      []     = grid
backtracking grid c gen seen traversed =
    if null neighbors_not_seen
    then backtracking grid previous_cell gen seen $ tail traversed
    else backtracking (connect grid c next_cell) next_cell gen' (S.insert next_cell seen) (c:traversed)
  where
    neighbors_not_seen = [cell | cell <- (neighbors grid c), cell `S.notMember` seen]
    (next_cell,gen') = randomPick neighbors_not_seen gen
    previous_cell = head traversed
