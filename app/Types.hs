--You know what Coords are
data Coord =(Float, Float)

--Avalible directions, maybe what to derive show or eq
-- Up is more like jump, so migh be a little more difficult to associate
data Directions = Right | Left | Up | Down

--A sheep fellow, maybe he will be purple, maybe he will be green
data Player= Player { heath ::Int,
                      pos :: Coord, 
                      fallingTime :: Float,
                      weapon :: Weapon,
                      money :: Int,
                      sMoneyAndSValubles :: [Item]
                      xVel :: Float,
                      yVel::Float}

-- Should be the same as player but w/out fallingTime?? and yVel 
data BadGuy = BadGuy {heath :: Int, 
                      pos::Coord, 
                      money::Int, 
                      pouch::[Item], 
                      xVel:: Float}

-- Your classic begener sheep 
starterSheep =Player {health=100, 
                     pos=(1,1),
                     fallingTime=0,
                     money=10, 
                     sMoneyAndSValubles=[Potion {quantEffect=10,descriptor="Health Potion +10", effect=Healing}],
                     xvel=0,
                     yvel=0}

data Items = potion Potion| weapon Weapon     -- Maybe a armor thing?

data Weapon = Weapon { wDamage :: Int       -- added to dmg rolls on hits
                     , wDesc   :: String}   --Exact descriptor, this would be for the users benefit


data Potion= Potion {quantEffect :: Int,  -- Like how much it increases your health/ xvel/yvel
                     descriptor:: String, --Exact descriptor, this would be for the users benefit
                     effect::Effect }     -- diff effect

data InvintorAction = DrinkPotion | EquipArmor | EquipWeapon

--          x/yvel ++  health++   health--   yVel--
data Effect = SpeedUp| Healing | Harming | SlowFalling   -- Ignore I stole this from minecwaft

data World = World {wSheep   :: Player      -- the player
                   , wLevel  :: Level      -- current game level
                   , wLevels :: [Level] }  -- all levels