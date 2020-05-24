import NumUtils

processInts :: NumModifier -> [Int] -> [Int]
processInts nm xs = map (run nm) xs

main :: IO ()
main = print $ processInts (NumModifier (+1)) [1,2,3]
