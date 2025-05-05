module FirstWorld where
import Types
firstWorld :: Float -> Float -> Float -> Float -> [Terrain]
firstWorld x y xs ys= [ Block x y, Block (x+(xs)) y,Block (x+(2.0*xs)) y, Block (x+(3.0*xs)) y, 
                           Block (x+(4.0*xs)) y,Block (x+(5.0*xs)) (y +(ys + (2.0*ys))), Block (x+(6.0*xs)) (y +(ys + (2.0*ys))), 
                           Block (x+(5.0*xs)) y, Block (x+(6.0*xs)) y, Block (x+(7.0*xs)) y, Block (x+(8.0*xs)) y,
                           Block  (x+(8.0*xs)) (y +(ys)), Block (x+(8.0*xs)) (y +(2*ys)), Block (x+(9.0*xs)) (y +(2*ys)),
                           Block (x+(10.0*xs)) (y +(2*ys)),Block (x+(10.0*xs)) (y +(ys)),Block (x+(10.0*xs)) (y),
                           Block (x+(11.0*xs)) (y),Block (x+(12.0*xs)) (y), Block (x+(13.0*xs)) (y),
                           Block (x+(14.0*xs)) (y),Block (x+(14.0*xs)) (y+ys),Block (x+(14.0*xs)) (y+(ys*2)),
                           Block (x+(15.0*xs)) (y+(ys*2)),Block (x+(16.0*xs)) (y+(ys*2)), Block (x+(16.0*xs)) (y+ys),
                           Block (x+(16.0*xs)) (y),Block (x+(17.0*xs)) (y), Block (x+(18.0*xs)) (y),
                           Block (x+(19.0*xs)) (y),Block (x+(20.0*xs)) (y),Block (x+(20.0*xs)) (y-ys),
                           Block (x+(20.0*xs)) (y-(2*ys)), Block (x+(20.0*xs)) (y-(3*ys)),Block (x+(23.0*xs)) (y),
                           Block (x+(23.0*xs)) (y-(ys)),Block (x+(23.0*xs)) (y-(ys*2)),Block (x+(23.0*xs)) (y-(ys*3)),
                           Block (x+(24.0*xs)) (y),Block (x+(25.0*xs)) (y), Block (x+(26.0*xs)) (y),
                           Block (x+(26.0*xs)) (y+(ys*2)),Block (x+(25.0*xs)) (y+(ys*4)),Block (x+(26.0*xs)) (y+(ys*6)),
                           Block (x+(27.0*xs)) (y),Block (x+(28.0*xs)) (y),Block (x+(28.0*xs)) (y+(ys)),
                           Block (x+(28.0*xs)) (y+(ys*2)), Block (x+(28.0*xs)) (y+(ys*3)),Block (x+(28.0*xs)) (y+(ys*4)),
                           Block (x+(28.0*xs)) (y+(ys*5)),Block (x+(28.0*xs)) (y+(ys*6)),Block (x+(29.0*xs)) (y+(ys*6)),
                           Block (x+(30.0*xs)) (y+(ys*6)),Block (x+(31.0*xs)) (y+(ys*6)),Block (x+(32.0*xs)) (y+(ys*6)),
                           Block (x+(33.0*xs)) (y+(ys*6)),Block (x+(34.0*xs)) (y+(ys*6)),Block (x+(35.0*xs)) (y+(ys*6)),
                           Block (x+(36.0*xs)) (y+(ys*6)),Block (x+(36.0*xs)) (y+(ys*5)),Block (x+(36.0*xs)) (y+(ys*4)),
                           Block (x+(36.0*xs)) (y+(ys*3)),Block (x+(36.0*xs)) (y+(ys*2)),Block (x+(36.0*xs)) (y+(ys*2)),
                           Block (x+(36.0*xs)) (y+(ys)),Block (x+(36.0*xs)) (y),Block (x+(36.0*xs)) (y-(ys)),
                           Block (x+(36.0*xs)) (y-(ys*2)),Block (x+(36.0*xs)) (y-(ys*3)), Block (x+(39.0*xs)) (y+(ys*7)),
                           Block (x+(40.0*xs)) (y+(ys*7)),Block (x+(43.0*xs)) (y+(ys*7)),Block (x+(44.0*xs)) (y+(ys*7)),
                           Block (x+(47.0*xs)) (y+(ys*5)),Block (x+(48.0*xs)) (y+(ys*5)),Block (x+(51.0*xs)) (y+(ys*6)),
                           Block (x+(52.0*xs)) (y+(ys*6)),Block (x+(55.0*xs)) (y+(ys*7)),Block (x+(58.0*xs)) (y+(ys*7)),
                           Block (x+(61.0*xs)) (y+(ys*7)),Block (x+(62.0*xs)) (y+(ys*7)),Block (x+(65.0*xs)) (y+(ys*7)),
                           Block (x+(66.0*xs)) (y+(ys*7)),Block (x+(67.0*xs)) (y+(ys*7)),Block (x+(68.0*xs)) (y+(ys*7)),
                           Block (x+(69.0*xs)) (y+(ys*7)),Block (x+(72.0*xs)) (y+(ys*7)),Block (x+(73.0*xs)) (y+(ys*7)),
                           Block (x+(75.0*xs)) (y+(ys*9)),Block (x+(76.0*xs)) (y+(ys*9)),Block (x+(78*xs)) (y+(ys*7)),
                           Block (x+(79.0*xs)) (y+(ys*7)),Block (x+(81.0*xs)) (y+(ys*5)),Block (x+(82.0*xs)) (y+(ys*5)),
                           Block (x+(85.0*xs)) (y+(ys*3)),Block (x+(86.0*xs)) (y+(ys*3)),Block (x+(87.0*xs)) (y+(ys*3)),
                           Block (x+(90.0*xs)) (y+(ys*3)),Block (x+(91.0*xs)) (y+(ys*3)), Block (x+(93.0*xs)) (y),
                           Block (x+(94.0*xs)) (y),Block (x+(95.0*xs)) (y),Block (x+(96.0*xs)) (y),Block (x+(97.0*xs)) (y),
                           Block (x+(98.0*xs)) (y),Block (x+(96.0*xs)) (y+(ys*3)),Block (x+(97.0*xs)) (y+(ys*3)),
                           Block (x+(98.0*xs)) (y+(ys*3)),Block (x+(99.0*xs)) (y),Block (x+(100.0*xs)) (y),Block (x+(101.0*xs)) (y),
                           Block (x+(102.0*xs)) (y),Block (x+(102.0*xs)) (y-(ys)),Block (x+(102.0*xs)) (y-(2*ys)),
                           Block (x+(102.0*xs)) (y-(ys*3)),Block (x+(102.0*xs)) (y-(ys*4)),Block (x+(102.0*xs)) (y-(ys*5)),
                           Block (x+(105.0*xs)) (y+(ys)),Block (x+(106.0*xs)) (y+(ys)),Block (x+(108.0*xs)) (y+(ys*2)),
                           Block (x+(109.0*xs)) (y+(ys*2)),Block (x+(111.0*xs)) (y+(ys*4)),Block (x+(112.0*xs)) (y+(ys*4)),
                           Block (x+(114.0*xs)) (y+(ys*6)),Block (x+(115.0*xs)) (y+(ys*6)),Block (x+(114.0*xs)) (y+(ys*2)),
                           Block (x+(115.0*xs)) (y+(ys*2)),Block (x+(117.0*xs)) (y+(ys*4)),Block (x+(118.0*xs)) (y+(ys*4)),
                           Block (x+(120.0*xs)) (y+(ys*3)),Block (x+(122.0*xs)) (y+(ys*2)),Block (x+(124.0*xs)) (y+(ys*3)),
                           Block (x+(126.0*xs)) (y+(ys*4)), Block (x+(128.0*xs)) (y+(ys*4)),Block (x+(129.0*xs)) (y+(ys*4)),
                           Block (x+(130.0*xs)) (y+(ys*4)),Block (x+(131.0*xs)) (y+(ys*4)),Block (x+(132.0*xs)) (y+(ys*4)),
                           Block (x+(129.0*xs)) (y+(ys*6)),Block (x+(130.0*xs)) (y+(ys*6)),Block (x+(131.0*xs)) (y+(ys*6)),
                           Block (x+(134.0*xs)) (y+(ys*4)),Block (x+(135.0*xs)) (y+(ys*4)),Block (x+(137.0*xs)) (y+(ys*2)),
                           Block (x+(138.0*xs)) (y+(ys*2)),Block (x+(140.0*xs)) (y),Block (x+(141.0*xs)) (y),
                           Block (x+(142.0*xs)) (y),Block (x+(143.0*xs)) (y),Block (x+(143.0*xs)) (y+(ys)),Block (x+(143.0*xs)) (y+(ys*2)),
                           Block (x+(144.0*xs)) (y+(ys*2)),Block (x+(145.0*xs)) (y+(ys*2)),Block (x+(146.0*xs)) (y+(ys*2)),
                           Block (x+(146.0*xs)) (y+(ys*2)),Block (x+(146.0*xs)) (y+(ys*3)),Block (x+(146.0*xs)) (y+(ys*4)),
                           Block (x+(147.0*xs)) (y+(ys*4)),Block (x+(148.0*xs)) (y+(ys*4)),Block (x+(149.0*xs)) (y+(ys*4)),
                           Block (x+(150.0*xs)) (y+(ys*4)),Block (x+(150.0*xs)) (y+(ys*5)),Block (x+(150.0*xs)) (y+(ys*6)),
                           Block (x+(151.0*xs)) (y+(ys*6)),Block (x+(152.0*xs)) (y+(ys*6)),Block (x+(153.0*xs)) (y+(ys*6)),
                           Block (x+(154.0*xs)) (y+(ys*6)),Block (x+(155.0*xs)) (y+(ys*6)),Block (x+(155.0*xs)) (y+(ys*5)),
                           Block (x+(155.0*xs)) (y+(ys*4)),Block (x+(156.0*xs)) (y+(ys*4)),Block (x+(157.0*xs)) (y+(ys*4)),
                           Block (x+(158.0*xs)) (y+(ys*4)),Block (x+(158.0*xs)) (y+(ys*3)),Block (x+(158.0*xs)) (y+(ys*2)),
                           Block (x+(158.0*xs)) (y+(ys*1)),Block (x+(158.0*xs)) (y),Block (x+(159.0*xs)) (y),
                           Block (x+(160.0*xs)) (y),Block (x+(161.0*xs)) (y),Block (x+(161.0*xs)) (y),
                           Block (x+(162.0*xs)) (y),Block (x+(163.0*xs)) (y),Block (x+(164.0*xs)) (y),
                           Block (x+(165.0*xs)) (y),Block (x+(166.0*xs)) (y),Block (x+(167.0*xs)) (y),
                           Block (x+(168.0*xs)) (y),Block (x+(169.0*xs)) (y),Block (x+(170.0*xs)) (y),Flag (x+(170*xs)) (y + ys)]

firstWorldToLevelBlock :: [Terrain] -> [JBlock]
firstWorldToLevelBlock [] = []
firstWorldToLevelBlock (Block x y : xs) =  reverse((JBlock (x, y) 10.5 40.5 None (HitBox (x1,y1) (x2,y1) (x1,y2) (x2,y2))): firstWorldToLevelBlock xs) -- currently implemented
    where 
        x1=x -50  --offset
        x2=x +54
        y1=y+25
        y2=y-35
{-
where 
    (x1, y2) = topLeft block
    x2 = x1 + width block
    y1 = y2 - height block

-}
-- (2*x1-50-offs) (2*x2-22-offs) && inBetween y ((2)*y1) (2*y2 + 80)
-- firstWorldToLevelBlock (Block x y : xs) =  reverse((JBlock (x, y) 30 53 None): firstWorldToLevelBlock xs)
firstWorldToLevelBlock (x :xs) = firstWorldToLevelBlock xs

flagToPoint :: [Terrain] -> (Float,Float)
flagToPoint [] = (100000,1000000)
flagToPoint (Flag x y : xs) = (x,y)
flagToPoint (x : xs) = flagToPoint xs

firstWorldToLevelCloud :: [Terrain] -> [Terrain]
firstWorldToLevelCloud [] = []
firstWorldToLevelCloud (Cloud x y v: xs ) = reverse(Cloud {cxpos = x , cypos = y, cvel = v} : firstWorldToLevelCloud xs)
firstWorldToLevelCloud (x : xs) = firstWorldToLevelCloud xs

firstWorldToLevelLava :: [Terrain] -> [Terrain]
firstWorldToLevelLava [] = []
firstWorldToLevelLava (Lava x y b : xs) = reverse (Lava {lxpos = x, lypos = y, fireBall = b} : firstWorldToLevelLava xs)
firstWorldToLevelLava (x: xs) = firstWorldToLevelLava xs

firstWorldToLevel :: [Terrain] -> Level
firstWorldToLevel terrain = Level {
  terrain = (firstWorldToLevelBlock terrain),
  clouds  = (firstWorldToLevelCloud terrain),
  lava    = (firstWorldToLevelLava terrain),
  enemies = baddies, -- Commented out, cause don't want baddies for now
  -- enemies = [],
  flag    = (flagToPoint terrain)
  }
  where
    baddies = [
      angel,angel2 ,angel3, god, 
      (makeAngel (2150, 100) (2150, -150) 0 (-2))
      ]

makeAngel :: (Float, Float) -> (Float, Float) -> Float -> Float -> BadGuy
makeAngel (x0, y0) goalXY velX velY = BadGuy {
    health_bad  = 10,
    money_bad   = 1,
    pouch       = [],
    pathing     = path,
    baddieBox   = makeAngelHitbox x0 y0,
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

angel2 :: BadGuy
angel2 = angel {
  pathing = basicAngelPath2,
  baddieBox = makeAngelHitbox (x basicAngelPath2) (y basicAngelPath2)
  }
angel3 :: BadGuy
angel3 = angel {
  pathing = basicAngelPath3,
  baddieBox = makeAngelHitbox (x basicAngelPath3) (y basicAngelPath3)
  }

god :: BadGuy
god = BadGuy {
  health_bad = 100,
  money_bad = 100, -- ?
  pouch = [],
  pathing = godPath,
  baddieBox = makeAngelHitbox 200 (-200),
  attack = Empty,
  isBoss = True
}

angel ::BadGuy
angel=
  BadGuy{
    health_bad=10,
    money_bad=1,
    pouch=[],
    pathing= basicAngelPath,
    baddieBox = makeAngelHitbox 100 (-100),
    attack=Empty,
    isBoss = False
}
-- Yes this is how we're doing this, sorry guys
--                   x       y
makeAngelHitbox :: Float ->Float ->HitBox
makeAngelHitbox x y = makeHitbox (x-70) (y+40) 160 80


basicAngelPath= 
  JPath{
    initPos=(200,(-100)),
    goalPos =(-100,(-100)),
    x=100,
    y=(-100),
    xVelocity=(-0.5),
    yVelocity=0
  }

basicAngelPath2= 
  JPath{
    initPos=(300,(0)),
    goalPos =(600,(300)),
    x=300,
    y=(0),
    xVelocity=(0.5),
    yVelocity=(0.5)
  }

basicAngelPath3= 
  JPath{
    initPos=(900,(0)),
    goalPos =(1200,(0)),
    x=900,
    y=(0),
    xVelocity=(0.5),
    yVelocity=(0)
  }

godPath = 
  JPath {
    initPos = (15550, 150),
    goalPos = (16000, 150),
    x = 15550,
    y = 150,
    xVelocity = 1,
    yVelocity = 0
  }