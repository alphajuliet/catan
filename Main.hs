{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
-- Main.hs
-- aj 2015-12-30

import System.Random
import System.Environment
import qualified Catan
import Diagrams.Prelude
import Diagrams.Backend.SVG (renderSVG)

------------------------------------
-- |Command dispatcher
dispatch :: [(String, [String] -> IO ())]
dispatch = [ 
  ("gen-board", printBoard),
  ("draw-board", renderBoard) ]

-- |Main loop
main = do 
  (command:args) <- getArgs
  let (Just action ) = lookup command dispatch
  action args 

------------------------------------
-- |Print out the board as a string
printBoard :: [String] -> IO ()
printBoard _  = print Catan.genBoard

dimensions :: SizeSpec V2 Double
dimensions = mkSizeSpec2D (Just 400) (Just 400)

-- |Display a hex of a given colour and type
renderBoard :: [String] -> IO ()
renderBoard _ = renderSVG "board.svg" dimensions Catan.drawBoard
-- The End
