module Test where
data Field = Air | Terrain
instance Show (Field) where
  show Air = show '-'
  show Terrain = show 'x'

charString :: Char -> Int -> String 
charString c length = [c | _ <- [1..length]]

contains :: Eq a => [a] -> a -> Bool
contains [] x = False
contains (y:ys) x = if y == x then True else contains ys x

makeString :: Char -> Char -> [Int] -> Int -> String
makeString c1 c2 is length = foldr go [] [0..length]
  where 
    go (i) recur = (if is `contains` i then c2 else c1) : recur

makeYLevel :: a -> a -> [Int] -> Int -> [a]
makeYLevel a b is length = foldr go [] [0..length]
  where 
    go (i) recur = (if is `contains` i then b else a) : recur
-- USE: 
-- ghci> makeYLevel (Air) (Terrain) [0, 5, 10] 10
-- ['x','-','-','-','-','x','-','-','-','-','x']

-- charAt :: Char -> [Int] -> String
-- charAt c is = 