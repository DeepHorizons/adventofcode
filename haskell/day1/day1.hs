{-|
--- Day 1: Not Quite Lisp ---
Santa was hoping for a white Christmas, but his weather machine's "snow" function is powered by stars, and he's fresh out! To save Christmas, he needs you to collect fifty stars by December 25th.
Collect stars by helping Santa solve puzzles. Two puzzles will be made available on each day in the advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!
Here's an easy puzzle to warm you up.
Santa is trying to deliver presents in a large apartment building, but he can't find the right floor - the directions he got are a little confusing. He starts on the ground floor (floor 0) and then follows the instructions one character at a time.
An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ), means he should go down one floor.
The apartment building is very tall, and the basement is very deep; he will never find the top or bottom floors.
For example:
    (()) and ()() both result in floor 0.
    ((( and (()(()( both result in floor 3.
    ))((((( also results in floor 3.
    ()) and ))( both result in floor -1 (the first basement level).
    ))) and )())()) both result in floor -3.
To what floor do the instructions take Santa?
-}

-- Take the sum of a list of integers of 1's and -1's that represent '(' and ')'
partOne1 xs = sum [if x == '(' then 1 else -1 | x <- xs]
-- Add to an acumulator the representational value of '(' and ')'
partOne2 xs = foldl (\acc x -> acc + if x == '(' then 1 else -1) 0 xs


{-|
--- Part Two ---

Now, given the same instructions, find the position of the first character that causes him to enter the basement (floor -1). The first character in the instructions has position 1, the second character has position 2, and so on.

For example:

    ) causes him to enter the basement at character position 1.
    ()()) causes him to enter the basement at character position 5.

What is the position of the character that causes Santa to first enter the basement?

-}

-- Get the list of numbers from the input
partOneList xs = [if x == '(' then 1 else -1 | x <- xs]

-- Go through the list of numbers and get the current count. If it is -1, stop
partTwoHelper xs acc
    | (acc + x) == -1 = []
    | otherwise = x : partTwoHelper (tail xs) (acc + x)
    where x = head xs

-- Get the lenght of the list from the helper function
partTwo1 xs = length (partTwoHelper (partOneList xs) 1)

-- Take the length of the resulting list of accumulator values if that ends if the accumulator is equal to -1
partTwo2 xs = length (takeWhile (/= (-1)) (scanl (\acc x -> acc + if x == '(' then 1 else -1) 0 xs))
