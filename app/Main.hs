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
  god <- loadBMP "resources/God.bmp"
  lightningBall <- loadBMP "resources/LightningBall.bmp"
  sheepMagic <- loadBMP "resources/SheepMag3.bmp"
  demonMagic <- loadBMP "resources/DemonMagic.bmp"
  swingEffectLeft <- loadBMP "resources/SwingEffectLeft.bmp"
  swingEffectRight <- loadBMP "resources/SwingEffectRight.bmp"
  gameOver <- loadBMP "resources/gameOver.bmp"

  let heavenList = [(scale 2 2 floorbmp),sheep,clouds,(Scale 2 2 angelGuy),heart, 
                    sheepSwing, sheepLeft,sheepLeftSwing,heavenback, 
                    (Scale 3 3 god), (Scale 2 2 lightningBall), (Scale 2 2 sheepMagic),
                    swingEffectLeft, swingEffectRight]

  let purgList= [(scale 2 2 purgFloor),sheep,fluGuy,(Scale 2 2 evilguy),heart,
                  sheepSwing,sheepLeft,sheepLeftSwing,mossback, 
                  (Scale 2 2 evilguy), (Scale 2 2 evilguy), (Scale 2 2 sheepMagic), 
                  swingEffectLeft, swingEffectRight]    

  let hellList= [(scale 2 2 hellfloor), sheep, fluGuy,(Scale 2 2 fluGuy), heart, 
                  sheepSwing,sheepLeft,sheepLeftSwing,(scale 2.75 3 hellback), 
                  machoMan, (Scale 2 2 demonMagic), (Scale 2 2 sheepMagic),
                  swingEffectLeft, swingEffectRight] 

  let levelResources = [heavenList, hellList,[Scale 2 2 gameOver]]           
  play
    (InWindow "GameEvent" (1000, 900) (0,0))   --Display mode
    black                                     -- in Init.hs
    fps                                        -- in Init.hs
    initWorld                                  -- in Init.hs
    (\world -> (worldToPicture world levelResources)) --A function to convert the world a picture.
    newHandleEvent                                -- in EventHandler.hs
    tick                                       -- in Tick.hs

worldToPicture:: World -> [[Picture]]->Picture
worldToPicture w picss = if hth<0 then (picss!!2)!!0 else
  pictures(bgrnd : 
          (drawPlayer h offset' pPics) :
          (pictures (drawFloor bs offset' (pics!!0))) :
          drawIntro w : 
          (drawHeart (pics!!4) w) ++
          drawEnimies bgs offset' (baddiePics)) 
  where 
    pics = picss !! (levelIndex w)
    offset' = (offset w)      -- The offset of the world
    bs = terrain (curLevel w) -- The JBlocks of this level
    h = hero w                -- current player info
    hth=health h
    bgs = enemies (curLevel w)   -- baddies
    pPics = (pics !! 11) : (getPlayPics h pics) -- allows for multiple player pictures
    bgrnd=(pics!!8)
    baddiePics = [pics !! 3, pics !! 9, pics !! 10]

getPlayPics :: Player -> [Picture] -> [Picture]
getPlayPics p pics = 
  if active (weapon p) 
    then case facingEast of 
      True ->  (pics !! 5) : [pics !! 13] -- Swinging to right
      False -> (pics !! 7) : [pics !! 12] -- swinging to left
  else 
    case facingEast of
       True -> [pics !! 1] -- facing right
       False -> [pics !! 6] -- facing left
  where 
    facingEast = facingRight p


--Draws multiple enimies but in the current form, with only one picture
drawEnimies :: [BadGuy]->Float->[Picture]->[Picture]
drawEnimies [] _ _ =[]
drawEnimies (bg:bgs) offs pics = 
  if (health_bad bg >0) then (image:attackPic++ (drawEnimies bgs offs pics)) else drawEnimies bgs offs pics -- :hitBox
  where 
    pic = if isBoss bg then head (tail pics) else head pics
    x0 = x (pathing bg)
    y0=y (pathing bg)
    image =(Translate (x0-offs) y0 pic)
    attackPic=(drawMagic (attack bg) (pics !! 2) offs) -- lightning ball
    pts = [
      (adjustX offs (topLt (baddieBox bg))),
      (adjustX offs (topRt (baddieBox bg))),
      (adjustX offs (bottomLt (baddieBox bg))),
      (adjustX offs (bottomRt (baddieBox bg)))]
    hitBox = (line pts)
    -- hitCircle = (translate xCenter yCenter (circle (hitRadius bg))) 

-- Draws the hearts, this should stay consistent throughout any level
drawHeart :: Picture -> World ->[Picture]
drawHeart pic w= go (health (hero w)) pic 
  where 
    go n _ =if n<1 then [] else (Translate ((n*80)+170) 380 pic) :go (n-1) pic
   

-- draws the text introduction
drawIntro :: World ->Picture
drawIntro w = draw x
  where
    num =intro w
    x= if (num `div` 150) >5 then 5 else (num `div` 150)
    draw x = (Translate (-450- (offset w)) 0 (scale 0.65 1 (text ((inTheBegining w)!!x))))
    
--draws player, with the offset
drawPlayer :: Player -> Float ->  [Picture] -> Picture
drawPlayer h offs pics = pictures ( translate adjustedX y pic : swingPic : (drawMagic (magic h) magicPic offs))  --weaponHBox : pHitbox:
  where
    pic = head (tail pics)
    magicPic = head pics
    y = yPos h
    x = (xPos h) - offs
    adjustedX = if facingRight h then x else (x - 30) -- THIS CODE SHIFTS Sheep
    w = weapon h -- The weapon
    -- swingEffect = if active w then pics !! 2 else Blank --TODO: make this more beautiful
    swingEffect = Blank
    swingPic = 
      if facingRight h 
        then Translate (adjustedX + 30) (y + 10) swingEffect 
      else   Translate (adjustedX - 30) (y + 10) swingEffect
    pt = [
      (adjustX offs (topLt (hitBox h))),
      (adjustX offs (topRt (hitBox h))),
      (adjustX offs (bottomLt (hitBox h))),
      (adjustX offs (bottomRt (hitBox h)))]
    weaponPts = [
      (adjustX offs (topLt (wHBox w))),
      (adjustX offs (topRt (wHBox w))),
      (adjustX offs (bottomLt (wHBox w))),
      (adjustX offs (bottomRt (wHBox w)))
      ]
    pHitbox= line pt
    weaponHBox = Line weaponPts

drawMagic :: Projectiles ->Picture ->Float-> [Picture]
drawMagic Empty _ _ =[]
drawMagic p pic offs = [(Translate x y (Rotate r(scale 0.5 0.5 pic)))] --projHitbox
  where
    r= (durration  p)*15
    x=(x1+x2)/2 -offs
    y=(y1+y2)/2
    (x1,y1)=(topLt (projBox p))
    (x2,y2)=(bottomRt (projBox p))
    pt = [
      (adjustX offs (topLt (projBox p))),
      (adjustX offs (topRt (projBox p))),
      (adjustX offs (bottomLt (projBox p))),
      (adjustX offs (bottomRt (projBox p)))]
    projHitbox= line pt
--(Line [((2*x1-50), ((2)*y1)), ((2*x2-22), (2*y2))]) :  -- these nums are NOT choosen arbitrarely
                              --(Line[((2*x0-50), ((2)*y1)), ((2*(x0+w)-22), (2*y2))]):
                              -- the 2* is for the scalar of the block
                              -- (-50) is how many pixels off the left side of the block picture doesnt take up
                              -- (-22) is how many pixels the right side of the block picture doesnt take up
                              --If you change this, it will break collison in its current form
drawFloor :: [JBlock] -> Float -> Picture -> [Picture] 
drawFloor [] offs pic = []
drawFloor (b:bs) offs pic = floorPic:drawFloor bs offs pic     --floorHBox                       
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
      (adjustX offs (topLt (floorBox b))),
      (adjustX offs (topRt (floorBox b))),
      (adjustX offs (bottomLt (floorBox b))),
      (adjustX offs (bottomRt (floorBox b)))]
    floorHBox = Line pts

adjustX ::Float->(Float,Float)->(Float,Float)
adjustX offs (x,y)=((x-offs),y)
   


--(2*x1-50-offs) (2*x2-22-offs)
--the block is actually -25 on left -21 on right so 54 pixels