-- Catan.hs
-- aj 2015-12-30

import System.Random
import System.Environment
import qualified GenBoard as Gen
import qualified DrawBoard as Draw
-- import Diagrams.Backend.SVG.CmdLine

------------------------------------
-- Command dispatcher
dispatch :: [(String, [String] -> IO ())]
dispatch = [ 
  ("board", board),
  ("hex", hex) ]

-- Main loop
main = do 
  (command:args) <- getArgs
  let (Just action ) = lookup command dispatch
  action args 

------------------------------------
-- Print out the board as a string
board :: [String] -> IO ()
board _  = print Gen.board

-- Display a hex of a given colour and type
hex :: [String] -> IO ()
hex _ = print "hex"

-- The End
