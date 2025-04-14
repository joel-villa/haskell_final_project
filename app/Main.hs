{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brillo
import Types
import EventHandler
import Tick
import Init

-- | Display the last event received as text.
--prelude.show.event, very helpful

--worldToPicture' World -> Picture->Picture->Picture 


main :: IO ()
main =do
  bmp <- loadBMP "resources/Sheep.bmp"
  floorbmp <- loadBMP "resources/pinkGrass.bmp"
  clouds <- loadBMP "resources/patchOfClouds.bmp"

  play
    (InWindow "GameEvent" (1000, 900) (0,0))   --Display mode
    backgroundColor                            -- in Init.hs
    fps                                        -- in Init.hs
    initWorld                                  -- in Init.hs
    (\world -> (worldToPicture world [floorbmp,bmp, clouds])) --A function to convert the world a picture.
    newHandleEvent                                -- in EventHandler.hs
    tick                                       -- in Tick.hs

worldToPicture:: World -> [Picture]->Picture
worldToPicture w pics = pictures((drawPlayer w (pics!!1)):(drawFloor w (pics!!0)) ++ (drawExtras w (pics!!2)))
  where 
    drawExtras w pic =(Scale 2.5 2.5 (Translate (-(getOffset (xPos (hero w)))) 100 pic)):[Scale 1.5 1 (Translate (-300-(getOffset (xPos (hero w)))) 300 pic)]     -- Sorry I will fix this later

drawPlayer :: World -> Picture -> Picture
drawPlayer world pic = Translate (20*((xPos (hero world)+ xVel (hero world))-(getOffset (xPos (hero world))))) (20*(yPos(hero world))) (pic)



drawFloor :: World->Picture -> [Picture]
drawFloor w pic   = case (floorpos(curLevel w)) of 
  [] -> []
  ((x,y):fs) -> floorPic x y w : drawFloor w{curLevel= ((curLevel w){floorpos=fs})} pic
  where
    floorPic x y w =scale 2 2 (Translate (5*(x-(getOffset (xPos (hero w))))) (5*y) (pic))  -- Draws the floor, scales each tile by 2X2







