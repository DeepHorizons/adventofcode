-- This can be compiled as it was too slow in the interpreter
-- ghc -O2 -o day6-p1 day6-p1.hs
{-
--- Day 6: Probably a Fire Hazard ---

Because your neighbors keep defeating you in the holiday house decorating contest year after year, you've decided to deploy one million lights in a 1000x1000 grid.
Furthermore, because you've been especially nice this year, Santa has mailed you instructions on how to display the ideal lighting configuration.
Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.
To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

For example:

    turn on 0,0 through 999,999 would turn on (or leave on) every light.
    toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
    turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

After following the instructions, how many lights are lit?

-}
import Data.List.Split
import Data.List

-- Describe the box here
maxX = 1000
maxY = 1000
totalSize = maxX*maxY

--This code was suuuuper slow
{-
-- Given two pairs of cordinates, proeuce a box with Treu in between the two coordinates with size maxX by maxY
genMask (x1, y1) (x2, y2) = replicate (maxY*y1) False ++ genMaskHelper (x1,y1) (x2, y2) ++ replicate ( maxY * ((maxY-1) - y2) ) False

genMaskHelper (x1, y1) (x2, y2) = take (maxY*(y2 - y1 + 1)) (cycle (replicate x1 False ++ replicate (x2 - x1 + 1) True ++ replicate (maxX - 1 - x2) False))

-- Make the bolean value True whereever the mask is True
turnOn [] [] = []
turnOn (x:box) (y:mask) = (x || y) : turnOn box mask

-- Make the boolean value False whereever the maks is True
turnOff [] [] = []
turnOff (x:box) (y:mask) = (x && (not y)) : turnOff box mask

-- Toggle the light whereever the mask is True
toggle [] [] = []
toggle (x:box) (y:mask) = (x /= y) : toggle box mask

getNewLights command box xs ys = case command of "turn on " -> turnOn box $ genMask xs ys
                                                 "turn off " -> turnOff box $ genMask xs ys
                                                 "toggle " -> toggle box $ genMask xs ys

-}
baseOp _ [] _ _ 0 = []
baseOp command (light: lights) (x1, y1) (x2, y2) lengthOfLights
  | index < (y1*maxY + x1) || index > (y2*maxY + x2) = light: baseOp command lights (x1, y1) (x2, y2) (lengthOfLights-1)
  | xSpace >= x1 && xSpace <= x2 = (command light) : baseOp command lights (x1, y1) (x2, y2) (lengthOfLights-1)
  | otherwise = light: baseOp command lights (x1, y1) (x2, y2) (lengthOfLights-1)
  where index = totalSize - lengthOfLights
        xSpace = index `mod` maxY

turnOn = baseOp (|| True)
turnOff = baseOp (&& False)
toggle = baseOp (not)

getNewLights command lights xs ys = case command of "turn on " -> turnOn lights xs ys totalSize
                                                    "turn off " -> turnOff lights xs ys totalSize
                                                    "toggle " -> toggle lights xs ys totalSize

-- Given a string of "<x>,<y>" make a 2 tuple
stringToTuple2 xs = listToTuple2 $  map (\x -> read x :: Int) $ splitOn "," xs
listToTuple2 xs = (xs !! 0, xs !! 1)

partOneHelper :: [Bool] -> IO Int
partOneHelper lights = do
    line <- getLine
    if null line
        then do
          let result = foldl (\acc x -> acc + if x then 1 else 0) 0 lights
          print result
          return result
        else do
          let tuples = map stringToTuple2 $ splitOn " through " $ dropWhile (\x -> x `notElem` ['0'..'9']) line
          let command = takeWhile (\x -> x `notElem` ['0'..'9']) line
          partOneHelper (getNewLights command lights (tuples !! 0) (tuples !! 1))

main = partOneHelper $ replicate (totalSize) False
