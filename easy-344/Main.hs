main :: IO [Int]
main = do
    inp <- readLn
    return (baum (inp :: Int))

getBin :: Int -> String
getBin 1 = "1"
getBin 0 = "0"
getBin n = getBin (quot n 2) ++ (if mod n 2 == 0 then "0" else "1")

baum1 :: String -> Int -> Int
baum1 "0" n = if mod n 2 == 1 then 1 else 0
baum1 "1" n = if mod n 2 == 1 then 0 else 1
baum1 (x:xs) n
    | x=='0' = baum1 xs (n+1)
    | n `mod` 2==1 = 0
    | otherwise = baum1 xs 0

--generates the Baum-Sweet sequence from 0 to some number n.
baum :: Int -> [Int]
baum n = 1 : map (\x -> baum1 (getBin x) 0) [1..n]
