-- gen-board.hs
-- Generate a Settlers of Catan standard board

module GenBoard (
  Resource,
  tiles,
  stdMap,
  board
  )
where

import Data.List
import System.Random
import System.Random.Shuffle

-- Define the contents of each tile
data Resource = Resource Integer String deriving (Show)

-- Define the available tiles
tileNumbers = [2, 3, 3, 4, 4, 5, 5, 6, 6, 8, 8, 9, 9, 10, 10, 11, 11, 12]
tileResources = ["brick", "wheat", "wood", "sheep", "ore"]
tileResourcesQ = [3, 4, 4, 4, 3]

-- Enumerate all the resources based on their quantity
enumerate :: [Integer] -> [a] -> [a]
enumerate n = concat . zipWith replicate (fromInteger <$> n)

resourcesDist = enumerate tileResourcesQ tileResources

genTiles :: [Integer] -> [String] -> [Resource]
genTiles = zipWith Resource

-- Randomly shuffle a list
-- @@TODO Replace with a real random source
shuffleList :: [a] -> [a]
shuffleList xs = shuffle' xs (length xs) (mkStdGen 1)

-- Generate all the tiles and shuffle them
tiles = genTiles tileNumbers (shuffleList resourcesDist) ++ [(Resource 7 "desert")] 

-- Define the standard hex board of 19 hexes in rows of 3, 4, 5, 4 and 3.
-- See http://www.redblobgames.com/grids/hexagons for details of the axial
-- coordinate system used.
stdMap = [
  (0, -2), (1, -2), (2, -2),
  (-1, -1), (0, -1), (1, -1), (2, -1),
  (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0),
  (-2, 1), (-1, 1), (0, 1), (1, 1),
  (-2, 2), (-1, 2), (0, 2)
  ]

-- Create the full board by randonly assigning a tile to each hex
board = zip stdMap (shuffleList tiles)

-- The End
