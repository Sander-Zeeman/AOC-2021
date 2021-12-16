hex2bin :: String -> String
hex2bin = concat . map hexHelper
  where
    hexHelper :: Char -> String
    hexHelper '0' = "0000"
    hexHelper '1' = "0001"
    hexHelper '2' = "0010"
    hexHelper '3' = "0011"
    hexHelper '4' = "0100"
    hexHelper '5' = "0101"
    hexHelper '6' = "0110"
    hexHelper '7' = "0111"
    hexHelper '8' = "1000"
    hexHelper '9' = "1001"
    hexHelper 'A' = "1010"
    hexHelper 'B' = "1011"
    hexHelper 'C' = "1100"
    hexHelper 'D' = "1101"
    hexHelper 'E' = "1110"
    hexHelper 'F' = "1111"

translateBinary :: String -> Integer
translateBinary [] = 0
translateBinary (b:bs) = read [b] + 2 * translateBinary bs

bin2dec :: String -> Integer
bin2dec = translateBinary . reverse

readValue :: String -> (Integer, String)
readValue ('0':str) = (bin2dec (take 4 str), drop 4 str)
readValue ('1':str) = (val + bin2dec (take 4 str), rem)
  where (val, rem) = readValue (drop 4 str)

handleNSubPackets :: [Integer] -> Integer -> String -> ([Integer], String)
handleNSubPackets values 0 str = (values, str)
handleNSubPackets values count str = handleNSubPackets (values ++ [value]) (count - 1) rem
  where
    (value, rem) = readPacket str

handleSubPackets :: [Integer] -> Integer -> String -> ([Integer], String)
handleSubPackets values 0 str = (values, str)
handleSubPackets values len str = handleSubPackets (values ++ [value]) (len - fromIntegral (length str - length rem)) rem
  where
    (value, rem) = readPacket str

output :: Integer -> [Integer] -> Integer
output 0 = sum
output 1 = product
output 2 = minimum
output 3 = maximum
output 5 = (\[x, y] -> if x > y then 1 else 0)
output 6 = (\[x, y] -> if x < y then 1 else 0)
output 7 = (\[x, y] -> if x == y then 1 else 0)

readPacket :: String -> (Integer, String)
readPacket str
  | pType == 4 = readValue $ drop 6 str
  | pID == 0 = (output pType valuesA, rema)
  | pID == 1 = (output pType valuesB, remb)
  where
    pType = bin2dec $ take 3 $ drop 3 str
    pID = bin2dec $ take 1 $ drop 6 str

    subLength = bin2dec $ take 15 $ drop 7 str
    (valuesA, rema) = handleSubPackets [] subLength (drop 22 str)

    subCount = bin2dec $ take 11 $ drop 7 str
    (valuesB, remb) = handleNSubPackets [] subCount  (drop 18 str)

main :: IO()
main = interact
  $ (\x -> x ++ "\n")
  . show
  . map readPacket
  . map hex2bin
  . lines