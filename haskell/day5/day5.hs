{-
--- Day 5: Doesn't He Have Intern-Elves For This? ---

Santa needs help figuring out which strings in his text file are naughty or nice.
A nice string is one with all of the following properties:

    It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
    It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
    It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.

For example:

    ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
    aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
    jchzalrnumimnmhp is naughty because it has no double letter.
    haegwjzuvuyypxyu is naughty because it contains the string xy.
    dvszwmarrgswjxmb is naughty because it contains only one vowel.

How many strings are nice?

-}

-- If the string contains 3 or more vowels, return True
containsAtleastThreeVowels xs = if (length $ filterVowels xs ) >= 3 then True else False

-- Filter out all of the vowels out of a string
filterVowels xs = filter (\x -> or $ map (==x) listOfVowels) xs
  where listOfVowels = "aeiou"

-- return True if the string contains a double letter
containsDoubleLetter [] = False
containsDoubleLetter (x:[]) = False
containsDoubleLetter (x:y:ys) = if x == y then True else containsDoubleLetter (y:ys)

doesNotContain listOfBad [] = True
doesNotContain listOfBad xs = if or $ map (\x -> take (length x) xs == x) listOfBad then False else doesNotContain listOfBad (tail xs)

doesNotContain_ab_cd_pq_xy = doesNotContain ["ab", "cd", "pq", "xy"]

partOneHelper niceStrings = do
    line <- getLine
    if null line
        then return niceStrings
        else do
            let isNice = and $ map ($ line) [containsAtleastThreeVowels, containsDoubleLetter, doesNotContain_ab_cd_pq_xy] -- apply the same argument to all three functions
            partOneHelper (if isNice then niceStrings + 1 else niceStrings)

partOne = partOneHelper 0

{-
--- Part Two ---

Realizing the error of his ways, Santa has switched to a better model of determining whether a string is naughty or nice. None of the old rules apply, as they are all clearly ridiculous.
Now, a nice string is one with all of the following properties:

    It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
    It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.

For example:

    qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter that repeats with exactly one letter between them (zxz).
    xxyxx is nice because it has a pair that appears twice and a letter that repeats with one between, even though the letters used by each rule overlap.
    uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter between them.
    ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but no pair that appears twice.

How many strings are nice under these new rules?

-}

-- Does the string contain a pair that does not overlap
containsPairNoOverlap [] = False
containsPairNoOverlap (x:[]) = False
containsPairNoOverlap (x:y:ys) = if or $ map (==(x:y:[])) $ substringOfLenght 2 ys then True else  containsPairNoOverlap (y:ys)

-- Produce every substring of length n
substringOfLenght n [] = []
substringOfLenght n xs = (take n xs) : substringOfLenght n (tail xs)

repeatWithOneLetterBetween [] = False
repeatWithOneLetterBetween (x:[]) = False
repeatWithOneLetterBetween (x:y:[]) = False
repeatWithOneLetterBetween (x:y:ys) = if x == (ys !! 0) then True else repeatWithOneLetterBetween (y:ys)

isStringNice string = and $ map ($ string) [containsPairNoOverlap, repeatWithOneLetterBetween] -- apply the same argument to all three functions

partTwoHelper niceStrings = do
    line <- getLine
    if null line
        then return niceStrings
        else do
            let isNice = isStringNice line
            partTwoHelper (if isNice then niceStrings + 1 else niceStrings)

partTwo = partTwoHelper 0
