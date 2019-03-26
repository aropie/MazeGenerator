import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)
type Node = S.Set Coord
type Maze = M.Map Coord Node

generateNewMaze :: Int -> Int -> Maze
generateNewMaze n m = M.fromList [((x,y),S.empty) | x <- [0..n-1], y <- [0..m-1]]

connect :: Maze -> Coord -> Coord -> Maze
connect g a b = M.adjust (S.insert b) a $ M.adjust (S.insert a) b g
