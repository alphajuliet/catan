-- Catan.hs
-- aj 2015-12-30

import System.Random
import System.Environment
import Catan
-- import Diagrams.Backend.SVG.CmdLine

------------------------------------
-- Command dispatcher
dispatch :: [(String, [String] -> IO ())]
dispatch = [ 
  ("gen-board", board),
  ("hex", hex) ]

-- Main loop
main = do 
  (command:args) <- getArgs
  let (Just action ) = lookup command dispatch
  action args 

------------------------------------
-- Print out the board as a string
board :: [String] -> IO ()
board _  = print genBoard

-- Display a hex of a given colour and type
hex :: [String] -> IO ()
hex _ = print "hex"

-- The End
