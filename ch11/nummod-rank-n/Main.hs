import NumUtils

processInts :: NumModifier -> [Int] -> [Int]
processInts nm xs = map (run nm) xs

main = print $ processInts (NumModifier (+1)) [1,2,3]
