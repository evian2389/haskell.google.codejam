
main = do
  interact $ unlines . zipWith (++) ["Case #" ++ show t ++ ": " | t <- [1..]] . solve . tail . lines


solve::[String] -> [String]
solve [] = []
solve (v:_:i:xs) = (findLoc value itemValues : solve xs)
  where value = read v
        itemValues = map read (words i)

findLoc:: Int -> [Int] -> String
findLoc _ [] = []
findLoc x _
  | x < 1 = []
findLoc v i = head [ show x ++ " " ++ show y | x <- [1..len],  y <- [x+1..len], x < y, i!!(x-1) + i!!(y-1) == v ]
        where len = length i
