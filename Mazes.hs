
module Mazes where

import qualified Data.Map as M
import qualified Data.Set as S
import System.Random (StdGen, randomR, mkStdGen)

type Coord = (Int, Int)
type Cell = S.Set Coord
type Grid = M.Map Coord Cell

generateNewMaze :: Int -> Int -> Grid
generateNewMaze n m = M.fromList [((x,y),S.empty) | x <- [0..n-1], y <- [0..m-1]]

connect :: Grid -> Coord -> Coord -> Grid
connect g a b = M.adjust (S.insert b) a $ M.adjust (S.insert a) b g

getBounds :: Grid -> Coord
getBounds g = f $ unzip $ M.keys g
  where f (a,b) = (maximum a, maximum b)

neighbors :: Grid -> Coord -> [Coord]
neighbors g (x,y) = filter p $ foldr (\(a,b) acc -> (a+x,b+y):acc) [] [(0,1),(0,-1),(1,0),(-1,0)]
  where
    max_x = fst $ getBounds g
    max_y = snd $ getBounds g
    p (a,b) = not $ a < 0 || b < 0 || a > max_x || b > max_y

drawCell :: Grid -> Coord -> String
drawCell g (x,y) = (if down `S.member` connected then "  " else "__") ++
                   (if right `S.member` connected then "_" else "|")
  where
    connected = g M.! (x,y)
    down = (x,y-1)
    right = (x+1,y)

drawRow :: Grid -> Int -> String
drawRow g r = foldr (++) "" [drawCell g (x,r) | x <- [0..max_x]]
  where
    max_x = fst $ getBounds g

drawMaze :: Grid -> String
drawMaze g = foldl (++) top_wall ["|" ++ (drawRow g r) ++ "\n" | r <- [max_y, max_y-1..0]]
  where
    max_y = snd $ getBounds g
    max_x = (fst $ getBounds g) + 1
    top_wall = "_" ++ (foldr (++) "" $ replicate max_x "___") ++ "\n"

randomPick :: [a] -> StdGen -> (a, StdGen)
randomPick xs gen =
  let
    (rand, gen') = randomR (0, length xs-1) gen
  in (xs!!rand,gen')

backtracking :: Grid -> StdGen -> Grid
backtracking grid gen =
  backtracking' grid (x,y) gen2 (S.singleton (x,y)) [(x,y)]
  where
    (max_x, max_y) = getBounds grid
    (x, gen1) = randomR (0, max_x) gen
    (y, gen2) = randomR (0, max_y) gen1

backtracking' :: Grid -> Coord -> StdGen -> S.Set Coord -> [Coord] -> Grid
backtracking' grid _  _   _      []     = grid
backtracking' grid c gen seen traversed =
    if null neighbors_not_seen
    then backtracking' grid previous_cell gen seen $ tail traversed
    else backtracking' (connect grid c next_cell) next_cell gen' (S.insert next_cell seen) (c:traversed)
  where
    neighbors_not_seen = [cell | cell <- (neighbors grid c), cell `S.notMember` seen]
    (next_cell,gen') = randomPick neighbors_not_seen gen
    previous_cell = head traversed

huntKill :: Grid -> StdGen -> Grid
huntKill grid gen =
  randomWalk grid (x,y) gen2 (S.singleton(x,y))
  where
    (max_x, max_y) = getBounds grid
    (x, gen1) = randomR (0, max_x) gen
    (y, gen2) = randomR (0, max_y) gen1

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
