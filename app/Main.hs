{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brillo
import Types
import EventHandler
import Tick
import Init
import GHC.Float

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
worldToPicture w pics = pictures((drawPlayer h offset (pics!!1)) : (pictures (drawFloor bs offset (pics!!0))) : (drawExtras w (pics!!2)))
  where 
    offset = getOffset (xPos (hero w))
    bs = terrain (curLevel w)
    h = hero w 
    drawExtras w pic =(Scale 2.5 2.5 (Translate (-(getOffset (xPos (hero w)))) 100 pic)):[Scale 1.5 1 (Translate (-300-(getOffset (xPos (hero w)))) 300 pic)]     -- Sorry I will fix this later

drawPlayer :: Player -> Float ->  Picture -> Picture
drawPlayer h offset pic = pictures [translate x y pic, translate x y (circle 5)] --arbitrary 5, center is position of player
  where
    x0 = xPos h
    y = yPos h
    x = x0 - offset
-- drawPlayer world pic = Translate (20*((xPos (hero world))-(getOffset (xPos (hero world))))) ((yPos(hero world))) (pic)

drawFloor :: [JBlock] -> Float -> Picture -> [Picture] --TODO
drawFloor [] offset pic = []
drawFloor (b:bs) offset pic = floorPic : rectPic : drawFloor bs offset pic
  where 
    (x0, y) = topLeft b
    x = x0 - offset
    wdth = width b
    h = height b
    points = [(x, y), (x + wdth, y), (x + wdth, y - h), (x, y -h)]
    floorPic = translate x y pic
    rectPic = translate x y (polygon points)

-- drawFloor :: World->Picture -> [Picture]
-- drawFloor w pic = case (terrain(curLevel w)) of 
--   [] -> []
--   ((JBlock (x,y) _ _ _):bs) -> floorPic x y w : drawFloor w{curLevel= ((curLevel w){terrain=bs})} pic
--   where
--     floorPic x y w =scale 2 2 (Translate ((x-(getOffset (xPos (hero w))))) (y) (pic))  -- Draws the floor, scales each tile by 2X2







