{- Requires split pacakge -}
{-|
--- Day 2: I Was Told There Would Be No Math ---
The elves are running low on wrapping paper, and so they need to submit an order for more. They have a list of the dimensions (length l, width w, and height h) of each present, and only want to order exactly as much as they need.
Fortunately, every present is a box (a perfect right rectangular prism), which makes calculating the required wrapping paper for each gift a little easier: find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves also need a little extra paper for each present: the area of the smallest side.

For example:

    A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper plus 6 square feet of slack, for a total of 58 square feet.
    A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper plus 1 square foot of slack, for a total of 43 square feet.

All numbers in the elves' list are in feet. How many total square feet of wrapping paper should they order?

-}
import Data.List.Split
import Data.List


-- Create a list of tuples of the sizes of eachwall
boxAreaCreator :: [a] -> [(a, a)]
boxAreaCreator [_] = []
boxAreaCreator xs = (zip (repeat $ head xs) (tail xs)) ++ (boxAreaCreator (tail xs))

-- Get the area of the box object described in boxAreaCreator
boxAreaProduct xs = sum [2 * (fst x) * (snd x) | x <- boxAreaCreator xs]

--Get the area of the smallest wall
minArea xs = (sortedList !! 0) * (sortedList !! 1)
  where sortedList = sort xs

-- Get the area of wrappign paper required as defined by `Area of box + area of smallest side`
getArea xs = (boxAreaProduct xs) + (minArea xs)

-- For every line, compute the area of wrapping paper needed and recursivelty call the function with the current amount
-- A line is formatted by `AxBxZ`
-- To run, paste the list of boxes, one on each line, then enter in an empty line to return the result
partOne x = do
    line <- getLine
    if null line
        then return x
        else do
            let box = map (read::String->Int) $ splitOn "x" line
            partOne (x + (getArea box))

{-
--- Part Two ---

The elves are also running low on ribbon. Ribbon is all the same width, so they only have to worry about the length they need to order, which they would again like to be exact.

The ribbon required to wrap a present is the shortest distance around its sides, or the smallest perimeter of any one face. Each present also requires a bow made out of ribbon as well; the feet of ribbon required for the perfect bow is equal to the cubic feet of volume of the present. Don't ask how they tie the bow, though; they'll never tell.

For example:

    A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap the present plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34 feet.
    A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon to wrap the present plus 1*1*10 = 10 feet of ribbon for the bow, for a total of 14 feet.

How many total feet of ribbon should they order?
-}

-- Get the smallest perimeter from the described edges
minPermieter xs = minimum [ 2 * ((fst x) + snd x) | x <- boxAreaCreator xs]

-- Calculate the ribbon perimeter from the perimeter
ribbonPerimeter xs = minPermieter xs

-- Get the volume of the box
volumeOfBox xs = product xs
ribbonVolume xs = volumeOfBox xs

--Calculate the total ribbon for the box given
totalRibbon xs = (ribbonVolume xs) + ribbonPerimeter xs

-- A line is formatted by `AxBxZ`
partTwo x = do
    line <- getLine
    if null line
        then return x
        else do
            let box = map (read::String->Int) $ splitOn "x" line
            partTwo (x + totalRibbon box)
