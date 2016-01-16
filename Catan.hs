{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{- |
Module      :  $Header$
Description :  Settlers of Catan board generation and analysis
License     :  None

Created     :  2015-12
Maintainer  :  andrew@alphajuliet.com
Stability   :  experimental

Generate a Settlers of Catan standard board, and do some analysis and visualisation
-}
module Catan (
  stdMap,
  stdHex,
  genBoard,
  drawBoard,
  scoreBoard
) where

import Data.List
import Data.Maybe
import System.Random
import System.Random.Shuffle
import qualified Data.Map as Map
import Diagrams.Prelude
import GHC.Exts

------------------------------------
-- Set up data structures

-- |Define the contents of each tile
data Resource = Brick | Grain | Wood | Wool | Ore | Desert deriving (Show, Ord, Eq)

-- |A hex with a resource and a number
data Hex = Hex { 
  number :: Integer,
  rsrc :: Resource
} deriving (Show)

-- |An alias for a 2D coordinate
type Coord = (Double, Double)

-- |A tile is a hex plus a location
data Tile = Tile Coord Hex deriving (Show)

------------------------------------

-- |Randomly shuffle a list
-- @@TODO Replace with a real random source
shuffleList :: [a] -> [a]
shuffleList xs = shuffle' xs (length xs) gen
  where gen = mkStdGen 42

-- |Generate all the hexes and shuffle them
hexes :: [Integer] -> [Resource] -> [Hex]
hexes nums dist = zipWith Hex nums (shuffleList dist) ++ [Hex 7 Desert] 

-- |Enumerate a list based on the given quantities
enumerate :: [Integer] -> [a] -> [a]
enumerate ns = concat . zipWith replicate (map fromInteger ns)

------------------------------------
-- |Define the standard hex board of 19 hexes in rows of 3, 4, 5, 4 and 3. 
-- See http://www.redblobgames.com/grids/hexagons for details of the axial
-- coordinate system used.
stdMap :: [Coord]
stdMap = [
  (0, -2), (1, -2), (2, -2),
  (-1, -1), (0, -1), (1, -1), (2, -1),
  (-2, 0), (-1, 0), (0, 0), (1, 0), (2, 0),
  (-2, 1), (-1, 1), (0, 1), (1, 1),
  (-2, 2), (-1, 2), (0, 2)]

-- |Define the available tiles
stdHex = hexes ns rs
  where ns = [2, 3, 3, 4, 4, 5, 5, 6, 6, 8, 8, 9, 9, 10, 10, 11, 11, 12]
        rs = enumerate count resources
        resources = [Brick, Grain, Wood, Wool, Ore]
        count = [3, 4, 4, 4, 3]

------------------------------------
-- |Create a full board by randomly assigning a hex to each location
genBoard :: [Coord] -> [Hex] -> [Tile]
genBoard coords hs = zipWith Tile coords $ shuffleList hs

------------------------------------
-- Draw the board
------------------------------------

-- |Convert hex axial coords to cartesian coords
-- See http://www.redblobgames.com/grids/hexagons
-- hexToXY :: Double t => t -> (t, t) -> (t, t)
hexToXY size (q, r) = ( size * sqrt 3 * (q + r/2), size * 1.5 * r )

------------------------------------
-- |Map resources to colours
type ColourMap = Map.Map Resource (Colour Double)

colourMap = Map.fromList [
    (Brick, orange)
  , (Grain, gold)
  , (Wood, green)
  , (Wool, lightgreen)
  , (Ore, lightblue)
  , (Desert, grey) ]

-- |Draw a hex of a given colour with a number in the middle
-- hex :: a -> Maybe (Colour Double) -> Diagram B
drawHex n colour =
  text (show n) #
    fontSizeL 0.8 #
    fc white <>
  hexagon 1 #
    fc (fromJust colour) #
    rotateBy (1/12)

-- |Draw a tile
drawTile (Hex n r) = drawHex n colour 
  where colour :: Maybe (Colour Double) 
        colour = Map.lookup r colourMap

-- |Draw a Catan board
drawBoard b = position (map mkShape b)
  where mkShape (Tile xy hex) = (p2 (hexToXY 1.0 xy), drawTile hex)


------------------------------------
-- Score the resources
------------------------------------

-- |Group the hexes by the type of resource
hexesByResource :: [Tile] -> [[Hex]]
hexesByResource board = groupWith rsrc $ allHexes board
  where allHexes = map (\(Tile _ h) -> h)

-- |Summarise the board hex numbers by resource
-- @@TODO Find a better way to do this
summarise :: [Tile] -> [(Resource, [Integer])]
summarise board = zip (f h) (g h)
  where f = map (rsrc . head)  -- extract the name of each resource
        g = map (map number)   -- extract the values of each hex by resource
        h = hexesByResource board           -- group tiles by resource

-- |Points for each hex value from 0 to 12. Note that there are no 0-hexes, and
-- that a 7 is a desert and therefore scores 0 points.
-- Note also that there is an extra zero at the start because Haskell array
-- indices start at zero and our first hex value is 2.
points = [0] ++ [0..5] ++ [0] ++ [5,4..1]

-- |Select elements from a target array given an array of indices
selectFromList :: [a] -> [Integer] -> [a]
selectFromList target = map ((target!!) . fromInteger)

-- |Score the resources across the board
scoreBoard :: (Enum b, Num b) => [Tile] -> [(Resource, b)]
scoreBoard board = zip (resources summ) (scores summ)
  where summ = summarise board
        resources = map fst
        scores = map $ (sum . selectFromList points) . snd

-- The End
