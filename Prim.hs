module Prim where

import Utils
import System.Random(StdGen, randomR)
import qualified Data.Set as S

generate :: Int -> Int -> StdGen -> Grid
generate wide tall gen =
  prim grid gen2 carved frontier
  where
    grid = generateNewMaze wide tall
    (max_x, max_y) = getBounds grid
    (initial_x, gen1) = randomR (0, max_x) gen
    (initial_y, gen2) = randomR (0, max_y) gen1
    carved = S.singleton (initial_x, initial_y)
    frontier = S.fromList $ neighbors grid (initial_x, initial_y)

-- prim ::
prim :: Grid -> StdGen -> S.Set Coord -> S.Set Coord -> Grid
prim grid gen carved frontier =
  if S.null frontier
  then grid
  else prim connected_maze gen2 new_carved new_frontier
  where
    (picked, gen1) = randomChoice frontier gen
    valid_cell_picks =  carved `S.intersection` (S.fromList $ neighbors grid picked)
    (carve_to, gen2) = randomChoice valid_cell_picks gen1
    connected_maze = connect grid picked carve_to
    new_carved = S.insert picked carved
    new_frontier = ((S.delete picked frontier) `S.union` (S.fromList $ neighbors grid picked)) `S.difference` new_carved
