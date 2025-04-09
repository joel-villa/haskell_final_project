{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brillo
--import Data.Text 
-- import Brillo.Interface.IO.Game (Event (EventKey))
import Types
import EventHandler
import Tick

-- | Display the last event received as text.
--prelude.show.event, very helpful

--worldToPicture' World -> Picture->Picture->Picture 
main :: IO ()

main =do
  bmp <- loadBMP "resources/LilDude.bmp"
  floorbmp <- loadBMP "resources/floor.bmp"

  play
    (InWindow "GameEvent" (1000, 1000) (10, 10))   --Display mode
    cyan    --Background color.
    100  --Number of simulation steps to take for each second of real time.
    (World(0, (0.5))([(-60,0),(-47,0),(-33,0),(-12,0),(-2,0),(11,0),(27,0),(49,0)]))   --The initial world.
    (\world -> (pictures (worldToPicture world bmp floorbmp))) --A function to convert the world a picture.
    handleEvent   -- (Event -> world -> world) A function to handle input events.
    tick --(Float -> world -> world)

-- data World = World { pos  :: (Float,Float)   }     -- current level depth}  -- all levels
worldToPicture :: World -> Picture->Picture-> [Picture]
worldToPicture (World(x,y) []) pic floorpic= [Translate (20*x) (20*y) (pic)] 
worldToPicture (World(x,y) ((z,w):fs)) pic floorpic = Translate (5*z) (5*w) (floorpic) : worldToPicture (World(x,y) (fs)) pic floorpic
--TODO make this capable of 

