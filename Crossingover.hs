module Crossingover where
import Common
import Random_
import Data.List

-- выбор типа кроссинговера
multiplexor_cross :: Int -> Int -> Double -> [Genotype] -> [Genotype]
multiplexor_cross type_c kol_c crs_p gen 
    | type_c == 1 = m_cross crs_p gen kol_c
    | otherwise = unified_cross crs_p gen 

-- "единый" кроссинговер
unified_cross :: Double ->[Genotype] -> [Genotype]
unified_cross _ [] = []
unified_cross _ (a:[]) = [a]
unified_cross p ((Gen bin1 dec1):(Gen bin2 dec2):t) 
    | c < p = (split_gens_unif bin1 bin2 [(Gen [] (-1)), (Gen [] (-1))])++(unified_cross p t)
    | otherwise = ((Gen bin1 dec1):(Gen bin2 dec2):[]) ++ (unified_cross p t)
    where
        c = random_double 0.0 1.0
-- изменение бинарника для единого кроссинговера
split_gens_unif :: [Int] -> [Int] -> [Genotype] -> [Genotype]
split_gens_unif [] [] gens = gens 
split_gens_unif (x:xs) (y:ys) [(Gen bin1 (-1)),(Gen bin2 (-1))] = split_gens_unif xs ys [(Gen bin11 (-1)),(Gen bin22 (-1))]
    where 
        bin11
            | c == 1 = bin1 ++ [x] 
            | c == 2 = bin1 ++ [y]
        bin22 
            | c == 2 = bin2 ++ [x] 
            | c == 1 = bin2 ++ [y]
        c = random_int 1 2

-- М-точечный кроссинговер
m_cross :: Double -> [Genotype] -> Int -> [Genotype]
m_cross _ [] _ = []
m_cross _ (a:[]) _ = [a]
m_cross p ((Gen bin1 dec1):(Gen bin2 dec2):t) m 
    | c < p = (cross preproc bin1 bin2 [(Gen [] (-1)), (Gen [] (-1))] 0) ++ (m_cross p t m)
    | otherwise = [(Gen bin1 dec1),(Gen bin2 dec2)] ++ (m_cross p t m)
        where
            c = random_double 0.0 1.0
            gener = random_list_int m 1 $ (length bin1)-1
            preproc = rmDup (sort gener)

-- список точек кросс, первый ген, второй, итоговой генотип, итерация, вывод
cross :: [Int] -> [Int] -> [Int] -> [Genotype] -> Int -> [Genotype]
cross _ [] [] gen _ = gen 
cross [] x y [(Gen bin1 (-1)),(Gen bin2 (-1))] i = [(Gen (bin1++x) (-1)),(Gen (bin2++y) (-1))]
cross (point:t) (x:xs) (y:ys) [(Gen bin1 (-1)),(Gen bin2 (-1))] i = cross r_point xs ys [(Gen bin11 (-1)),(Gen bin22 (-1))] (i+1)
    where 
        r_point
            | point == (i+1) = t
            | otherwise = (point:t)
        bin_1 = bin1 ++ [x] 
        bin_2 = bin2 ++ [y]
        bin11 
            | point == (i+1) = bin_2
            | otherwise = bin_1
        bin22 
            | point == (i+1) = bin_1
            | otherwise = bin_2

-- удалить дублирующиеся элементы
rmDup :: [Int] -> [Int] 
rmDup [] = []
rmDup (x:xs) = x : rmDup (filter (\y -> not(x == y)) xs)