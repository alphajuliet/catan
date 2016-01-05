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
  genBoard,
  drawBoard
) where

import Data.List
import System.Random
import System.Random.Shuffle
import qualified Data.Map as Map
import Diagrams.Prelude

------------------------------------
-- Set up data structures

-- |Define the contents of each tile
data Resource = Brick | Grain | Wood | Wool | Ore | Desert deriving (Show, Ord, Eq)
  
-- |A hex with a resource and a number
data Hex = Hex Integer Resource deriving (Show)

-- |An alias for a 2D coordinate
type Coord = (Double, Double)

-- |A tile is a hex plus a location
data Tile = Tile Coord Hex deriving (Show)

------------------------------------
-- Accessors
hexN :: Hex -> Integer
hexN (Hex n _) = n

hexR :: Hex -> Resource
hexR (Hex _ r) = r

tileXY :: Tile -> Coord
tileXY (Tile xy _) = xy

tileHex :: Tile -> Hex
tileHex (Tile _ h) = h

------------------------------------
-- |Define the available tiles
tileNumbers = [2, 3, 3, 4, 4, 5, 5, 6, 6, 8, 8, 9, 9, 10, 10, 11, 11, 12]
tileResources = [Brick, Grain, Wood, Wool, Ore]
tileResourcesQ = [3, 4, 4, 4, 3]

resourcesDist :: [Resource]
resourcesDist = enumerate tileResourcesQ tileResources
  where enumerate ns = concat . zipWith replicate (map fromInteger ns)

genHexes :: [Integer] -> [Resource] -> [Hex]
genHexes = zipWith Hex

-- |Randomly shuffle a list
-- @@TODO Replace with a real random source
shuffleList :: RandomGen g => [a] -> g -> [a]
shuffleList xs = shuffle' xs $ length xs 

-- |Generate all the tiles and shuffle them
tiles :: RandomGen g => [Resource] -> g -> [Hex]
tiles dist gen = genHexes tileNumbers (shuffleList dist gen) ++ [Hex 7 Desert] 

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

-- |Create the full board by randomly assigning a tile to each hex
genBoard :: [Tile]
genBoard = zipWith Tile 
  stdMap $ 
  shuffleList (tiles resourcesDist gen) gen
    where gen = mkStdGen 101


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

-- hex :: Int -> Colour Double -> Diagram B
-- |Draw a hex of a given colour with a number in the middle
drawHex n (Just colour) =
  text (show n) #
    fontSizeL 0.8 #
    fc white <>
  hexagon 1 #
    fc colour #
    rotateBy (1/12)

-- |Draw a tile
drawTile (Hex n r) = drawHex n colour 
  where colour = Map.lookup r colourMap

-- drawBoard :: [Tile] -> Diagram B
-- drawBoard [] = circle 0.05
-- drawBoard ((Tile xy (Hex n r)):xs) = drawTile (Hex n r) ||| drawBoard xs
drawBoard = position (map mkShape genBoard)
  where mkShape (Tile (x, y) hex) = (p2 (hexToXY 1.0 (x, y)), drawTile hex)

-- The End
