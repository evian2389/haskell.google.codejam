

main :: IO()
main = do
  interact $ unlines . zipWith (++) [ "Case #" ++ show t ++ ": " | t <- [1..]] . solve . lines

solve :: [String] -> [String]
solve ( _ldn :s ) =
  let
    [ _ , d , _ ] = map read $ ( words  _ldn )
    (ds , ps) = splitAt d s
    searchWord p = show . length $ patternMatch ds (parse p)
  in map searchWord ps
solve [] = []

parse :: [Char] -> [String]
parse [] = []
parse ('(':s) = let ( a, _:b) = break (==')') s in (a:parse b)
parse (a:as) = [a] : parse as

patternMatch :: [String] -> [String] -> [String]
patternMatch d p =
  filter ( and . zipWith (flip elem) p) d
