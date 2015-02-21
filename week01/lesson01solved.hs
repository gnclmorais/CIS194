-- Validating Credit Card Numbers
-- Steps:
--     1. Double the value of every second digit beggining from the right.
--     2. Sum all the digits (digits, as in, 16 will be 1 + 6).
--     3. Calculate the remainder when the sum is divided by 10.
--     4. If the remainder is 0, the result is valid.

-- Exercise 1
--     135 `mod` 10 -> 5
--     135 `div` 10 -> 13
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]
    
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Exercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []       = []
doubleEveryOther (x:[])   = [x]
doubleEveryOther (x:y:zs) = x : y * 2 : doubleEveryOther zs

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits (x:[]) = x
sumDigits (x:zs) = (x `div` 10) + (x `mod` 10 ) + sumDigits zs

-- Exercise 4
validate :: Integer -> Bool
validate x = mod (sumDigits (doubleEveryOther (toDigitsRev x))) 10 == 0

-- Exercise 5
-- The Towers of Hanoi
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi disks a b c = hanoi (disks - 1) a c b
                 ++ [(a, b)]
                 ++ hanoi (disks - 1) c b a

-- Main section
main = do
    print (validate 4012888888881881)
    print (validate 4012888888881882)
    
    print (hanoi 3 "a" "b" "c")
