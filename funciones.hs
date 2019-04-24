min3 a b c
  | (a <= b) && (a <= c) = a
  | (b <= a) && (b <= c) = b
  | otherwise = c

firstRow :: String -> Row
firstRow s = [0..(length s)]