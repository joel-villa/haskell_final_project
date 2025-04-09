{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brillo
--import Data.Text 
-- import Brillo.Interface.IO.Game (Event (EventKey))
import Types
import EventHandler
import Tick
import WorldToPic


-- | Display the last event received as text.
--prelude.show.event, very helpful
main :: IO ()

main =do
  bmp <- loadBMP "357BMPstuff/LilDude.bmp"

  let worldToPic = \w -> worldToPicture w bmp

  play
    (InWindow "GameEvent" (700, 100) (10, 10))   --Display mode
    white    --Background color.
    100  --Number of simulation steps to take for each second of real time.
    (World(0, 0 ))   --The initial world.
    worldToPic  --A function to convert the world a picture.
    handleEvent   -- (Event -> world -> world) A function to handle input events.
    tick --(Float -> world -> world)

-- data World = World { pos  :: (Float,Float)   }     -- current level depth}  -- all levels