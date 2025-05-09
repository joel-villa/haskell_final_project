module Types where

--Avalible directions, maybe what to derive show or eq
-- Up is more like jump, so migh be a little more difficult to associate
--data Directions = Right | Left | Up | Down

--A sheep fellow, maybe he will be purple, maybe he will be green
data Player= 
  Player { 
    xPos               :: Float, 
    yPos               :: Float, 
    xVel               :: Float,
    yVel               :: Float,
    health             :: Float, -- I made this a float because im lazy, sorry guys
    inAir              :: Bool,
    money              :: Int,
    sMoneyAndSValubles :: [Item],
    weapon             :: Item, 
    facingRight        :: Bool,
    hitBox:: HitBox,
    magic :: Projectiles,
    iDamage :: Float
  }


-- Should be the same as player but w/out fallingTime?? and yVel 
data BadGuy = 
  BadGuy {
    health_bad :: Int,  
    money_bad  :: Int, 
    pouch      :: [Item], 
    pathing    :: JPath,
    baddieBox  :: HitBox,
    attack     :: Projectiles,
    isBoss     :: Bool
  }


data JPath = 
  None
  | JPath {
    initPos      :: (Float, Float),
    goalPos      :: (Float, Float),
    x            :: Float,
    y            :: Float,
    xVelocity    :: Float,
    yVelocity    :: Float
  }

data Item = 
  Potion {
    quantEffect :: Int,    -- Like how much it increases your health/ xvel/yvel
    descriptor  :: String, -- Exact descriptor, this would be for the users benefit
    effect      :: Effect 
  }     -- diff effect
  | Weapon {
    wDamage     :: Int,            -- added to dmg rolls on hits
    wDesc       :: String,         -- Exact descriptor, this would be for the users benefit
    wVelocity   :: Float,          -- 0 when not active, decremented in Tick while active
    relativePos :: (Float, Float), -- relative to sheep's position
    active      :: Bool,           -- is this weapon currently being used?
    wHBox       :: HitBox
  }   
-- Maybe a armor thing?

data InvintorAction = DrinkPotion | EquipArmor | EquipWeapon

--          x/yvel ++  health++   health--   yVel--
data Effect = SpeedUp| Healing | Harming | SlowFalling   -- Ignore I stole this from minecwaft

data JBlock = 
  JBlock {
    topLeft     :: (Float, Float),
    height      :: Float,
    width       :: Float, 
    path        :: JPath,
    floorBox:: HitBox
  }

--I gave it all 4 points incase we dont want to not only be a box
data HitBox=
  HitBox {
    topLt:: (Float,Float),
    topRt::(Float,Float),
    bottomLt ::(Float,Float),
    bottomRt :: (Float, Float)
  }

data Projectiles=
  Projectiles {
    projBox ::HitBox,
    durration :: Float,
    direction :: Float,
    yDirection::Float
  } | Empty


data Level = 
  Level {
    terrain  :: [JBlock],
    floorpos :: [(Float,Float)], 
    clouds   :: [Terrain],
    lava     :: [Terrain],
    enemies  :: [BadGuy],
    flag     :: (Float, Float)
  } 
data World = 
  World { 
    hero            :: Player, 
    curLevel        :: Level, 
    offset          :: Float,
    intro           :: Int,
    inTheBegining   :: [String],
    levels          :: [Level],
    levelIndex      :: Int
    } -- TODO change this to other World
heavenIntro=["In the begining", "there was darkness", "And God said","Let there be a sheep","and then God said", "Go west young sheep"]
data Terrain = 
               Block       {bxpos :: Float, bypos :: Float}                  |
               Brick       {brxpos :: Float, brypos :: Float}                |
               Cloud       {cxpos :: Float, cypos :: Float, cvel:: Float}    | 
               Fence       {fxpos :: Float, fypos :: Float}                  |
               Lava        {lxpos :: Float, lypos :: Float, fireBall :: Bool}|
               MovingBlock {mbxpos :: Float, mbypos :: Float, mbpath :: JPath} |
               Flag        {fgxpos :: Float, fgypos :: Float}



--            leftX topY   width   height
makeHitbox :: Float->Float->Float->Float->HitBox
makeHitbox x y w h =HitBox (x,y) (x+w,y) (x,y-h) (x+w,y-h)

