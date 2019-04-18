
module Mazes where

import qualified Data.Map as M
import qualified Data.Set as S

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
