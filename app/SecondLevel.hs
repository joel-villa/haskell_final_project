module SecondLevel where
import Types
import FirstWorld

secondLevel :: Float -> Float -> Float -> Float -> [Terrain]
secondLevel x y xs ys = [ Block x y, Block (x+xs) y, Block (x+(2*xs)) y,
                         Block (x+(3*xs)) y,Block (x+(4*xs)) y,Block (x+(5*xs)) y,
                         Lava (x+(6*xs)) y False ,Lava (x+(7*xs)) y True, Lava (x+(8*xs)) y False,
                         Block (x+(9*xs)) y,Block (x+(10*xs)) y,Block (x+(11*xs)) y,
                         Block (x+(12*xs)) y, Block (x+(13*xs)) y, Lava (x+(14*xs)) y False,
                         Lava (x+(15*xs)) y True, Lava (x+(16*xs)) y False, Block (x+(17*xs)) y,
                         Block (x+(18*xs)) y,Block (x+(19*xs)) y,Block (x+(20*xs)) y,
                         Block (x+(22*xs)) (y+(3*ys)),Block (x+(23*xs)) (y+(3*ys)),Block (x+(24*xs)) (y+(3*ys)),
                         Block (x+(25*xs)) (y+(6*ys)),Block (x+(26*xs)) (y+(6*ys)),Block (x+(27*xs)) (y+(6*ys)),
                         Block (x+(28*xs)) (y+(3*ys)),Block (x+(29*xs)) (y+(3*ys)),Block (x+(30*xs)) (y+(3*ys)),
                         Block (x+(33*xs)) (y+(3*ys)),Block (x+(35*xs)) (y+(6*ys)),Block (x+(36*xs)) (y+(6*ys)),
                         Block (x+(38*xs)) (y+(3*ys)),Block (x+(40*xs)) (y+(6*ys)),Block (x+(42*xs)) (y+(3*ys)),
                         Block (x+(44*xs)) (y+(5*ys)),Block (x+(45*xs)) (y+(5*ys)),Block (x+(47*xs)) (y+(6*ys)),
                         Block (x+(48*xs)) (y+(6*ys)),Block (x+(49*xs)) (y+(3*ys)),Block (x+(50*xs)) (y+(3*ys)),
                         Block (x+(54*xs)) (y+(3*ys)),Block (x+(58*xs)) (y+(3*ys))]

makeMovingPath :: (Float,Float) -> (Float,Float) -> Float -> Float -> JPath
makeMovingPath (r,t) (a,b) xv yv = JPath {
    initPos = (r,t),
    goalPos = (a,b),
    x = r ,
    y = t,
    xVelocity = xv,
    yVelocity = yv
}

secondWorldToLevel :: [Terrain] -> Level
secondWorldToLevel terrain = Level {
  terrain = (firstWorldToLevelBlock terrain),
  clouds  = (firstWorldToLevelCloud terrain),
  lava    = (firstWorldToLevelLava terrain),
  --enemies = [angel,angel2 ,angel3]
  enemies = demons,
  flag    = (flagToPoint terrain)
  }

demons :: [BadGuy]
demons = [
  (makeDemon ( 500, -250) ( 500,    0) 0 1),
  (makeDemon (1300, -250) (1300,    0) 0 1),
  (makeDemon (1300, -350) (1300, -100) 0 1),
  (makeDemon (1300, -150) (1300,  100) 0 1),
  (makeDemon (1300,  -50) (1300,  200) 0 1),
  (makeDemon (1300, -450) (1300, -200) 0 1),
  (makeDemon (2500,  -50) (3100,  -50) 1 0),
  (makeDemon (2800,  -50) (3400,  -50) 1 0),
  (makeDemon (3100,  -50) (3700,  -50) 1 0),
  (makeDemon (3400,  -50) (4000,  -50) 1 0),
  (makeDemon (3700,  -50) (4300,  -50) 1 0)
  ]

makeDemon :: (Float, Float) -> (Float, Float) -> Float -> Float -> BadGuy
makeDemon (x0, y0) goalXY velX velY = BadGuy {
    health_bad  = 50, -- ? 
    money_bad   = 1,
    pouch       = [],
    pathing     = path,
    baddieBox   = makeDemonHitBox x0 y0,
    attack      = Empty,
    isBoss      = False
    }
  where 
    path = JPath {
      initPos   = (x0, y0),
      goalPos   = goalXY,
      x         = x0,
      y         = y0,
      xVelocity = velX,
      yVelocity = velY
    }

makeDemonHitBox :: Float ->Float ->HitBox
--                                leftX     topY    width   height
makeDemonHitBox x y = makeHitbox (x - 25) (y + 65)    60     70
