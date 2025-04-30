module Init where
import Brillo
import Types
import FirstWorld

-- fps: Number of simulation steps to take for each second of real time
fps :: Int 
fps = 60

-- The background color: 
backgroundColor :: Color
backgroundColor = (makeColor 0.75 0.75 1 0.5)


initWorld :: World
initWorld = World {
  hero = starterSheep,
  curLevel = (firstWorldToLevel(firstWorld (-200) (-160.0) 105 (60))),
  offset = 0,
  intro = 0,
  inTheBegining = heavenIntro,
  levels = [(firstWorldToLevel(firstWorld (-200) (-160.0) 105 (60)))],
  levelIndex = 0
}

-- Your classic begener sheep 
starterSheep :: Player
starterSheep =
  Player{
    xPos   = 0,
    yPos   = (0),
    xVel   = 0,
    yVel   = 0,
    health = 3, 
    inAir  = False,
    money  = 10, 
    sMoneyAndSValubles = [Potion {quantEffect=10,descriptor="Health Potion +10", effect=Healing}],
    weapon = Weapon 5 "initial sword" 18 0 (15, -25) False,
    facingRight = True,
    hitBox= HitBox ((10),(56)) ((20),(56)) ((10), (-31)) (20, (-31)),
    magic=Empty
  }

--so you give it the actual x and this is what you do with it
newHitBox:: Float-> Float-> Bool-> HitBox
newHitBox x y False= HitBox (x-5,y+20) ((x+45),y+20) (x-5,(y-62)) ((x+45),(y-62))
newHitBox x y True= HitBox (x-40,y+20) ((x+5),y+20) (x-40,(y-62)) ((x+5),(y-62))

updateHitboxB:: Float->Float->Bool->HitBox
--           bottomLeft
updateHitboxB x y False =HitBox ((x),(y+102)) ((x+100),(y+62)) ((x),y) ((x+100),y)
updateHitboxB x y True =HitBox ((x-50),(y+102)) ((x+10),(y+62)) ((x-50),y) ((x+10),y)

--so this needs the amount that you're moving it by
generalUpdateHitBox:: Float->Float->HitBox->HitBox
generalUpdateHitBox x y oldH= HitBox ((x1+x),(y1+y)) ((x2+x),(y1+y)) ((x1+x),(y2+y)) ((x2+x),(y2+y))
  where
    (x1,y1)= (topLt oldH)
    (x2,y2)= (bottomRt oldH)
