majorScales :: [String]
majorScales = ["C" ,"C#" ,"D" ,"D#" ,"E" ,"F" ,"F#" ,"G" ,"G#" ,"A" ,"A#" ,"B"]
doremi :: [String]
doremi = ["Do","Re","Mi","Fa","So","La","Ti"]
indices :: [Int]
indices = [0,2,4,5,7,9,11]

main :: IO (Maybe String)
main = do
    input <- getLine
    let f = words input
    case f of
        [a,b] -> if elem a majorScales && elem b doremi then return (Just (getNote a b)) else return Nothing
        _ -> return Nothing

getIdx :: (Eq a) => a -> [a] -> Int -> Int
getIdx _ [] _ = -1
getIdx str (x:xs) idx = if x==str then idx else getIdx str xs (idx+1)

getNote :: String -> String -> String
getNote n_scl n_sfg = majorScales !! ((idx+ p) `mod` 12)
                        where
                            idx = getIdx n_scl majorScales 0
                            p = indices !! getIdx n_sfg doremi 0
