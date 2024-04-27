
{-
Modify the binary string transmitter example to detect simple transmission errors using the concept
of parity bits. That is, each eight-bit binary number produced during encoding is extended with a
parity bit, set to one if the number contains an odd number of ones, and to zero otherwise. In turn,
each resulting nine-bit binary number consumed during decoding is checked to ensure that its parity
bit is correct, with the parity bit being discarded if this is the case, and a parity error being
reported otherwise.
Hint: the library function error :: String -> a displays the given string as an error message
and terminates the program; the polymorphic result type ensures that error can be used in any
context.
8.Test your new string transmitter program from the previous exercise using a faulty communication
channel that forgets the first bit, which can be modelled using the tail function on lists of bits.
-}

import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
  where
    weights = iterate (* 2) 1

bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> Bit
parity bits = sum bits `mod` 2

addParity :: [Bit] -> [Bit]
addParity bits = parity bits : bits

encode :: String -> [Bit]
encode = concatMap (addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity bits
  | head bits == parity (tail bits) = tail bits
  | otherwise = error "Parity Error"

decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chop9

channel :: [Bit] -> [Bit]
channel = id

transmit :: String -> String
transmit = decode . channel . encode