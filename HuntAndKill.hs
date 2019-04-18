module HuntAndKill where

import Utils
import System.Random (StdGen, randomR)
import qualified Data.Set as S


generate :: Int -> Int -> StdGen -> Grid
generate wide tall gen =
  randomWalk grid (x,y) gen2 (S.singleton(x,y))
  where
    (max_x, max_y) = getBounds grid
    (x, gen1) = randomR (0, max_x) gen
    (y, gen2) = randomR (0, max_y) gen1
    grid = generateNewMaze wide tall

randomWalk :: Grid -> Coord -> StdGen -> S.Set Coord -> Grid
randomWalk grid c gen seen =
  if null neighbors_not_seen
  then hunt grid (0,0) gen seen
  else randomWalk (connect grid c next_cell) next_cell gen' (S.insert next_cell seen)
  where
    neighbors_not_seen = [cell | cell <- (neighbors grid c), cell `S.notMember` seen]
    (next_cell, gen') = randomPick neighbors_not_seen gen

hunt :: Grid -> Coord -> StdGen -> S.Set Coord -> Grid
hunt grid c gen seen =
  case walk grid c of
    Just new_coord  -> if new_coord `S.notMember` seen && (not (null (neighbors_not_seen new_coord)))
                      then randomWalk grid new_coord gen (S.insert new_coord seen)
                      else hunt grid new_coord gen seen
    Nothing         -> grid
  where
    neighbors_not_seen coord = [cell | cell <- (neighbors grid coord), cell `S.notMember` seen]

walk :: Grid -> Coord -> Maybe Coord
walk g (x,y) = if x < max_x then Just (x+1,y) else
                   if y < max_y then Just (0,y+1) else Nothing
  where
    (max_x,max_y) = getBounds g
