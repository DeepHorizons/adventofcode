{-
--- Day 3: Perfectly Spherical Houses in a Vacuum ---

Santa is delivering presents to an infinite two-dimensional grid of houses.
He begins by delivering a present to the house at his starting location, and then an elf at the North Pole calls him via radio and tells him where to move next. Moves are always exactly one house to the north (^), south (v), east (>), or west (<). After each move, he delivers another present to the house at his new location.
However, the elf back at the north pole has had a little too much eggnog, and so his directions are a little off, and Santa ends up visiting some houses more than once. How many houses receive at least one present?

For example:

    > delivers presents to 2 houses: one at the starting location, and one to the east.
    ^>v< delivers presents to 4 houses in a square, including twice to the house at his starting/ending location.
    ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.
-}

-- Get the lenght of a unique list of all houses that santa visits
-- Input the directions via a string of ^>v<
partOne startingLoc directions = length $ uniqueLocations startingLoc directions [startingLoc]

--uniqueLocations :: (Num a) => (a,a) -> [Char] -> [(a,a)] -> [(a,a)]
uniqueLocations _ [] locList = locList
uniqueLocations loc (x:xs) locList = uniqueLocations nextLoc xs newLocList
  where newLocList = if nextLoc `elem` locList then locList else locList ++ [nextLoc]
        nextLoc = nextLocation loc x

nextLocation :: (Num a) => (a,a) -> Char -> (a,a)
nextLocation (x,y) direction = case direction of '>' -> (x+1, y)
                                                 '<' -> (x-1, y)
                                                 '^' -> (x, y+1)
                                                 'v' -> (x, y-1)

{-
--- Part Two ---

The next year, to speed up the process, Santa creates a robot version of himself, Robo-Santa, to deliver presents with him.
Santa and Robo-Santa start at the same location (delivering two presents to the same starting house), then take turns moving based on instructions from the elf, who is eggnoggedly reading from the same script as the previous year.
This year, how many houses receive at least one present?

For example:

    ^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa goes south.
    ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back where they started.
    ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction and Robo-Santa going the other.
-}

-- Take the length of the unique list of all houses visisted by santa and the robot
-- Input the directions via a string of ^>v<
partTwo startingLoc directions = length $ produceUniqueList robotVisitList santaVisitList
  where santaVisitList = uniqueLocations startingLoc santaDirections [startingLoc]
        robotVisitList = uniqueLocations startingLoc roboDirections [startingLoc]
        santaDirections = every 2 directions
        roboDirections = every 2 $ tail directions

-- Given one list, return a new list that only contains unique elements from both
produceUniqueList [] resultList = resultList
produceUniqueList (x:xs) resultList = produceUniqueList xs newResultList
  where newResultList = if x `elem` resultList then resultList else resultList ++ [x]

-- Produce a list with every nth element
every n xs
  | xs == [] = []
  | otherwise = head xs : (every n (drop n xs))
