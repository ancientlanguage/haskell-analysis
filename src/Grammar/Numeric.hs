module Grammar.Numeric where

maxDigitsDouble :: Integral a => [a] -> Double
maxDigitsDouble = logBase 10 . fromIntegral . maximum . (0 :)

maxDigits :: Integral a => [a] -> Int
maxDigits = ceiling . maxDigitsDouble
