module Main where

import System.Environment (getArgs)
import System.Random (getStdGen, StdGen)
import Utils

import qualified Backtracking
import qualified HuntAndKill
import qualified Prim

getGenerator :: Int -> (Int -> Int -> StdGen -> Grid)
getGenerator n = case n of
  1 -> Backtracking.generate
  2 -> HuntAndKill.generate
  3 -> Prim.generate
  _ -> error "Nope"

main :: IO()
main = do
  gen <- getStdGen
  [wide, tall, algo] <- map read <$> getArgs
  let
    generator = getGenerator algo
  putStrLn $ drawMaze $ generator wide tall gen
