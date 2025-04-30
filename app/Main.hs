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

main :: IO ()
main =do
  sheep <- loadBMP "resources/Sheep.bmp"
  floorbmp <- loadBMP "resources/pinkGrass.bmp"
  purgFloor <-loadBMP "resources/purgFloor.bmp"
  hellfloor <- loadBMP "resources/hellfloor.bmp"
  clouds <- loadBMP "resources/patchOfClouds.bmp"
  angelGuy <-loadBMP "resources/angleDude.bmp"
  heart<-loadBMP "resources/heart.bmp"
  sheepSwing <- loadBMP "resources/SheepSwing.bmp"
  sheepLeft <- loadBMP "resources/SheepLeft.bmp"
  sheepLeftSwing <- loadBMP "resources/SheepLeftSwing.bmp"
  hellback <-loadBMP "resources/hellBack.bmp"
  evilguy <- loadBMP "resources/LilDude.bmp"
  heavenback <-loadBMP "resources/heaven.bmp"
  machoMan <-loadBMP "resources/MachoMan.bmp"
  fluGuy <-loadBMP "resources/FlyGuy.bmp"
  mossback <- loadBMP "resources/mossForest.bmp"

  let heavenList=[(scale 2 2 floorbmp),sheep,clouds,angelGuy,heart,sheepSwing,sheepLeft,sheepLeftSwing,heavenback]

  let purgList=[(scale 2 2 purgFloor),sheep,fluGuy,evilguy,heart,sheepSwing,sheepLeft,sheepLeftSwing,mossback]    

  let hellList= [(scale 2 2 hellfloor), sheep, fluGuy,machoMan, heart,sheepSwing,sheepLeft,sheepLeftSwing,(scale 2.75 3 hellback)] 

  let levelResources = [heavenList, hellList]           
  play
    (InWindow "GameEvent" (1000, 900) (0,0))   --Display mode
    backgroundColor                            -- in Init.hs
    fps                                        -- in Init.hs
    initWorld                                  -- in Init.hs
    (\world -> (worldToPicture world levelResources)) --A function to convert the world a picture.
    newHandleEvent                                -- in EventHandler.hs
    tick                                       -- in Tick.hs

worldToPicture:: World -> [[Picture]]->Picture
worldToPicture w picss = 
  pictures(bgrnd: (drawPlayer h offset' pPic) : (pictures (drawFloor bs offset' (pics!!0))) :drawIntro w: (drawHeart (pics!!4) w)++ (drawExtras w (pics!!2))++drawEnimies bgs offset' (pics!!3)) 
  where 
    pics = picss !! (levelIndex w)
    offset' = (offset w)      -- The offset of the world
    bs = terrain (curLevel w) -- The JBlocks of this level
    h = hero w                -- current player info
    bgs = enemies (curLevel w)   -- baddies
    drawExtras w pic =(Scale 2.5 2.5 (Translate (0) 100 pic)):[Scale 1.5 1 (Translate (-300) 300 pic)]     -- Sorry I will fix this later
    pPic = getPlayPic h pics  -- allows for multiple player pictures
    bgrnd=(pics!!8)

getPlayPic :: Player -> [Picture] -> Picture
getPlayPic p pics = 
  if active (weapon p) 
    then case facingEast of 
      True -> pics !! 5  -- Swinging to right
      False -> pics !! 7 -- swinging to left
  else 
    case facingEast of
       True -> pics !! 1  -- facing right
       False -> pics !! 6 -- facing left
  where 
    facingEast = facingRight p


--Draws multiple enimies but in the current form, with only one picture
drawEnimies :: [BadGuy]->Float->Picture->[Picture]
drawEnimies [] _ _ =[]
drawEnimies (bg:bgs) offs pic = if (health_bad bg >0) then ((Translate (x0-offs) y0 (Scale 2 2 pic))):(hitBox:drawEnimies bgs offs pic) else drawEnimies bgs offs pic --
  where 
    x0 = x (pathing bg)
    y0=y (pathing bg)
    pts = [
      (unfloat offs (topLt (baddieBox bg))),
      (unfloat offs (topRt (baddieBox bg))),
      (unfloat offs (bottomLt (baddieBox bg))),
      (unfloat offs (bottomRt (baddieBox bg)))]
    hitBox = (line pts)
    -- hitCircle = (translate xCenter yCenter (circle (hitRadius bg))) 

-- Draws the hearts, this should stay consistent throughout any level
drawHeart :: Picture -> World ->[Picture]
drawHeart pic w= go (health (hero w)) pic 
  where 
    go 0.0 _ =[]
    go n pic =  (Translate ((n*80)+170) 380 pic) :go (n-1) pic

-- draws the text introduction
drawIntro :: World ->Picture
drawIntro w = draw x
  where
    num =intro w
    x= if (num `div` 150) >5 then 5 else (num `div` 150)
    draw x = (Translate (-450- (offset w)) 0 (scale 0.65 1 (text ((inTheBegining w)!!x))))
    
--draws player, with the offset
drawPlayer :: Player -> Float ->  Picture -> Picture
drawPlayer h offs pic = pictures ( translate adjustedX y pic : pHitbox:(drawMagic (magic h) pic offs))  --
  where
    y = yPos h
    x = (xPos h) - offs
    adjustedX = if facingRight h then x else (x - 30) -- THIS CODE SHIFTS Sheep
    playerPos = translate x y (circle (10)) -- arbitrary 10 (for player position)
    w = weapon h -- The weapon
    (relX,relY) = relativePos w  -- The weapon's position relative to sheep
    w_hit_rad = wDamageRadius w
    (wCenterX, wCenterY) = (x + relX, y + relY)
    (wx1, wy1) = (wCenterX - w_hit_rad, wCenterY + w_hit_rad)
    (wx2, wy2) = (wCenterX + w_hit_rad, wCenterY -w_hit_rad)
    pts = [(wx1, wy1), (wx2, wy1), (wx2, wy2), (wx1, wy2), (wx1, wy1)]
    pt = [
      (unfloat offs (topLt (hitBox h))),
      (unfloat offs (topRt (hitBox h))),
      (unfloat offs (bottomLt (hitBox h))),
      (unfloat offs (bottomRt (hitBox h)))]
    weaponHBox = line pts
    pHitbox= line pt
-- drawPlayer world pic = Translate (20*((xPos (hero world))-(getOffset (xPos (hero world))))) ((yPos(hero world))) (pic)

drawMagic :: Projectiles ->Picture ->Float-> [Picture]
drawMagic Empty _ _ =[]
drawMagic p pic offs = [(Translate x y (Rotate r(scale 0.5 0.5 pic))), projHitbox]
  where
    r= (durration  p)*15
    x=(x1+x2)/2 -offs
    y=(y1+y2)/2
    (x1,y1)=(topLt (projBox p))
    (x2,y2)=(bottomRt (projBox p))
    pt = [
      (unfloat offs (topLt (projBox p))),
      (unfloat offs (topRt (projBox p))),
      (unfloat offs (bottomLt (projBox p))),
      (unfloat offs (bottomRt (projBox p)))]
    projHitbox= line pt
--(Line [((2*x1-50), ((2)*y1)), ((2*x2-22), (2*y2))]) :  -- these nums are NOT choosen arbitrarely
                              --(Line[((2*x0-50), ((2)*y1)), ((2*(x0+w)-22), (2*y2))]):
                              -- the 2* is for the scalar of the block
                              -- (-50) is how many pixels off the left side of the block picture doesnt take up
                              -- (-22) is how many pixels the right side of the block picture doesnt take up
                              --If you change this, it will break collison in its current form
drawFloor :: [JBlock] -> Float -> Picture -> [Picture] 
drawFloor [] offs pic = []
drawFloor (b:bs) offs pic = floorPic:floorHBox:drawFloor bs offs pic                            
  where 
    (x0, yCenter) = topLeft b
    xCenter = x0 - offs
    wdth = width b
    hght = height b
    (x1, y1) = (xCenter - (wdth * 0.5) + 2, yCenter + (hght * 0.5) - 3) -- the top left corner 
    (x2, y2) = (x1 + wdth, y1 - hght) -- bottom right corner
    floorPic = ((translate xCenter yCenter pic))
    (scaled_x1, scaled_y1) = (x1*2, y1*2)
    (scaled_x2, scaled_y2) = (x2*2, y2*2)
    pts = [
      (unfloat offs (topLt (floorBox b))),
      (unfloat offs (topRt (floorBox b))),
      (unfloat offs (bottomLt (floorBox b))),
      (unfloat offs (bottomRt (floorBox b)))]
    floorHBox = Line pts

unfloat ::Float->(Float,Float)->(Float,Float)
unfloat offs (x,y)=((x-offs),y)
   


--(2*x1-50-offs) (2*x2-22-offs)
--the block is actually -25 on left -21 on right so 54 pixels