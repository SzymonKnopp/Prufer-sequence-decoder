contains :: [Integer] -> Integer -> Bool
contains list x = (sum [if n == x then 1 else 0 | n <- list]) >= 1

extract_snd :: [(Integer, Integer)] -> [Integer]
extract_snd tupleList = [snd x | x <- tupleList]

lowest_absent :: [Integer] -> Integer
lowest_absent list = lowest_absent' list 1 where
                     lowest_absent' list' n | (not (list' `contains` n)) = 1
                                            | otherwise = 1 + lowest_absent' list' (n+1)

new_edge :: [Integer] -> [(Integer, Integer)] -> (Integer, Integer)
new_edge pruferSeq edges = (head pruferSeq, lowest_absent (pruferSeq ++ (extract_snd edges)))

decode_Prufer :: [Integer] -> [(Integer, Integer)]
decode_Prufer pruferSeq = decode_Prufer' pruferSeq []
                        where
                            decode_Prufer' pruferSeq' edges
                              | (length pruferSeq' == 0)
                              = edges ++ [new_edge [lowest_absent (extract_snd edges)] edges]
                              | otherwise
                              = decode_Prufer'
                                  (tail pruferSeq') (edges ++ [new_edge pruferSeq' edges])

main :: IO ()
main = do print ("Program wypisuje zbior krawedzi w drzewie zdefiniowanym przez kod Prufera.")
          print ("Podaj kod Prufera w postaci listy:")
          n <- readLn
          print ("Wynik:")
          print (decode_Prufer (n :: [Integer]))