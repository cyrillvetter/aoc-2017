input = 289326

main = do
    print $ part1 input
    print $ part2 input

part1 :: Int -> Int
part1 num =
    let oddSquares = map (^2) $ filter odd [1..]
        rings = takeWhile (<= num) oddSquares
        ringAxes = take 4 $ dropWhile (<= last rings) generateAxes
    in length rings + minimum (map (\p -> abs (num - p)) ringAxes)

part2 :: Int -> Int
part2 num = 0

generateAxes :: [Int]
generateAxes = scanl (+) 1 $ gen 1
    where
        gen :: Int -> [Int]
        gen num = num : replicate 3 (num + 1) ++ gen (num + 2)