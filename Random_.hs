module Random_ where 
import System.Random
import System.IO.Unsafe

random_int :: Int -> Int -> Int
random_int a b = unsafePerformIO $ randomRIO(a,b) 

random_list_int :: Int -> Int -> Int -> [Int] 
random_list_int 0 _ _ = []
random_list_int n a b = (random_int a b):(random_list_int (n-1) a b)

random_double ::  Double -> Double -> Double
random_double a b = unsafePerformIO $ randomRIO(a,b)

random_init_gen :: Int -> [Int]
random_init_gen 0 = []
random_init_gen n = (random_int 0 1):(random_init_gen (n-1))