-- This can be compiled as it was too slow in the interpreter
-- ghc -O2 -o day6-p2 day6-p2.hs
{-
--- Part Two ---

You just finish implementing your winning light pattern when you realize you mistranslated Santa's message from Ancient Nordic Elvish.
The light grid you bought actually has individual brightness controls; each light can have a brightness of zero or more. The lights all start at zero.

The phrase turn on actually means that you should increase the brightness of those lights by 1.
The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.
The phrase toggle actually means that you should increase the brightness of those lights by 2.

What is the total brightness of all lights combined after following Santa's instructions?

For example:

    turn on 0,0 through 0,0 would increase the total brightness by 1.
    toggle 0,0 through 999,999 would increase the total brightness by 2000000.
-}
import Data.List.Split
import Data.List

-- Describe the box here
maxX = 1000
maxY = 1000
totalSize = maxX*maxY

baseOp _ [] _ _ 0 = []
baseOp command (light: lights) (x1, y1) (x2, y2) lengthOfLights
  | index < (y1*maxY + x1) || index > (y2*maxY + x2) = light: baseOp command lights (x1, y1) (x2, y2) (lengthOfLights-1)
  | xSpace >= x1 && xSpace <= x2 = (command light) : baseOp command lights (x1, y1) (x2, y2) (lengthOfLights-1)
  | otherwise = light: baseOp command lights (x1, y1) (x2, y2) (lengthOfLights-1)
  where index = totalSize - lengthOfLights
        xSpace = index `mod` maxY

turnOn = baseOp (+ 1)
turnOff = baseOp (\x -> if (x - 1) < 0 then 0 else (x - 1))
toggle = baseOp (+ 2)

getNewLights command lights xs ys = case command of "turn on " -> turnOn lights xs ys totalSize
                                                    "turn off " -> turnOff lights xs ys totalSize
                                                    "toggle " -> toggle lights xs ys totalSize

-- Given a string of "<x>,<y>" make a 2 tuple
stringToTuple2 xs = listToTuple2 $  map (\x -> read x :: Int) $ splitOn "," xs
listToTuple2 xs = (xs !! 0, xs !! 1)

partTwoHelper :: [Int] -> IO Int
partTwoHelper lights = do
    line <- getLine
    if null line
        then do
          let result = sum lights
          print result
          return result
        else do
          let tuples = map stringToTuple2 $ splitOn " through " $ dropWhile (\x -> x `notElem` ['0'..'9']) line
          let command = takeWhile (\x -> x `notElem` ['0'..'9']) line
          partOneHelper (getNewLights command lights (tuples !! 0) (tuples !! 1))

main = partTwoHelper $ replicate (totalSize) 0
