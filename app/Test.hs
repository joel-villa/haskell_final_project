module Test where
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
makeYLevel c1 c2 is length = foldr go [] [0..length]
  where 
    go (i) recur = (if is `contains` i then c2 else c1) : recur

-- charAt :: Char -> [Int] -> String
-- charAt c is = 