module Mutation where
import Common
import Random_

-- оператор мутации
mutation :: Double -> [Genotype] -> [Genotype]
mutation _ [] = []
mutation p ((Gen bin dec_bin):t)
    | c < p = (Gen new_bin (-1)):(mutation p t)
    | otherwise = (Gen bin dec_bin):(mutation p t)
        where
            c = random_double 0.0 1.0
            _ind = random_int 1 (length bin)
            new_bin = modif _ind bin 

-- для мутации, инверсия клетки
modif :: Int -> [Int] -> [Int]
modif _ [] = []
modif 1 (h:t)
    | h == 0 = 1:t 
    | otherwise = 0:t
modif n (h:t) = h : (modif (n-1) t)
