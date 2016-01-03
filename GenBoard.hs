-- GenBoard.hs
-- Generate a Settlers of Catan standard board

{- | Module to create a Catan board -}
module GenBoard (
  Resource,
  Hex,
  Coord,
  Tile,

  tiles,
  stdMap,
  board
  )
where

import Data.List
import System.Random
import System.Random.Shuffle

------------------------------------
{- | Define the contents of each tile -}
data Resource = Brick | Wheat | Wood | Wool | Ore | Desert deriving (Show)
data Hex = Hex Integer Resource deriving (Show)
type Coord = (Integer, Integer)
data Tile = Tile Coord Hex deriving (Show)

-- Accessors
hex_n (Hex n _) = n
hex_r (Hex _ r) = r
tile_xy (Tile xy _) = xy
tile_hex (Tile _ h) = h

{- | Define the available tiles -}
tileNumbers = [2, 3, 3, 4, 4, 5, 5, 6, 6, 8, 8, 9, 9, 10, 10, 11, 11, 12]
tileResources = [Brick, Wheat, Wood, Wool, Ore]
tileResourcesQ = [3, 4, 4, 4, 3]

{- | Enumerate all the resources based on their quantity -}
enumerate :: [Integer] -> [a] -> [a]
enumerate n = concat . zipWith replicate (fromInteger <$> n)

resourcesDist :: [Resource]
resourcesDist = enumerate tileResourcesQ tileResources

genHexes :: [Integer] -> [Resource] -> [Hex]
genHexes = zipWith Hex

{- | Randomly shuffle a list -}
-- @@TODO Replace with a real random source
shuffleList :: RandomGen g => [a] -> g -> [a]
shuffleList xs = shuffle' xs $ length xs 

{- | Generate all the tiles and shuffle them -}
tiles :: RandomGen g => [Resource] -> g -> [Hex]
tiles dist gen = genHexes tileNumbers (shuffleList dist gen) ++ [Hex 7 Desert] 

{- | Define the standard hex board of 19 hexes in rows of 3, 4, 5, 4 and 3. 
See http://www.redblobgames.com/grids/hexagons for details of the axial
coordinate system used. -}
stdMap :: [Coord]
stdMap = [
  (0, -2), (1, -2), (2, -2),
  (-1, -1), (0, -1), (1, -1), (2, -1),
  (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0),
  (-2, 1), (-1, 1), (0, 1), (1, 1),
  (-2, 2), (-1, 2), (0, 2)
  ]

{- | Create the full board by randomly assigning a tile to each hex -}
board :: [Tile]
board = zipWith Tile 
  stdMap $ 
  shuffleList (tiles resourcesDist gen) gen
  where gen = mkStdGen 100

-- The End
