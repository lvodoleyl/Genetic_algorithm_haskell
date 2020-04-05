module Common where 
import Random_
import Prelude

data Parametrs = P {populat::Int,       -- популяция
                    select::Int,        -- тип селекции
                    cross_t::Int,       -- тип кроссинговера
                    cross_n::Int,       -- точки кроссинговера
                    cross_p::Double,    -- вероятность кроссинговера
                    mutat_p::Double,    -- вероятность мутации
                    stop_t::Int,        -- критерий остановки
                    stop_n::Int}        -- значение критерия
    deriving Show
data Genotype = Gen{bin::[Int],
                    dec::Int} -- бинарь и десятичный бинарь
    deriving Show

data Fenotype = Fen{gen::Genotype,
                    fitness::Double, 
                    prob::Double} -- ген, значение фитнес-функ и вероятность
    deriving Show

data Optimize = Opt{down::Double,
                    up::Double,
                    delta::Double,
                    target::(Double -> Double)}
--  нижняя граница, верхняя граница, разница, фитнесс-функция

-- лучший фенотип
max_fen :: [Fenotype] -> Fenotype -> Fenotype
max_fen [] fen = fen 
max_fen ((Fen gen fitness prob):t) (Fen g fit_best p) 
    | fitness >= fit_best = max_fen t (Fen gen fitness prob)
    | otherwise = max_fen t (Fen g fit_best p) 

-- начальная генерация особей
init_gens :: Int -> Int -> [Genotype]
init_gens 0 _ = []
init_gens n kol = (Gen (random_init_gen kol) (-1)):(init_gens (n-1) kol)

-- перевод гена из двоичного вида в десятичный
bin_to_dec :: [Int] -> Int -> Int 
bin_to_dec [] _ = 0
bin_to_dec (h:t) m = h*m + (bin_to_dec t m*2)

-- формирование списка фенотипов 
list_fen :: [Genotype] -> Optimize -> [Fenotype]
list_fen [] _ = []
list_fen (gen:t) o = (create_fen gen o):(list_fen t o)

-- создание фенотипа и прогонки через фитнесс-функцию
create_fen :: Genotype -> Optimize -> Fenotype
create_fen (Gen bin bin_dec) (Opt down up delt func)
    | bin_dec == -1 = create_fen (Gen bin (bin_to_dec(reverse bin) 1)) (Opt down up delt func)
    | otherwise = Fen (Gen bin bin_dec) fit (-1.0)
        where
            fit = let a = down + delt*double_bin_dec in func a
            double_bin_dec = fromIntegral bin_dec

-- вытащить ген из фена
getGenFromFen :: Fenotype -> Genotype
getGenFromFen (Fen gen f p) = gen

-- вытащить конкретный фенотип из списка
getFenNumb :: [Fenotype] -> Int -> Fenotype
getFenNumb (f:t) 0 = f 
getFenNumb (f:t) n = getFenNumb t (n-1)

-- пузырьковая сортировка по возрастанию списка фенотипа по значению фитнесс-фенкции
go_bubble_sort_fen :: Int -> [Fenotype] -> [Fenotype]
go_bubble_sort_fen 0 fen = fen 
go_bubble_sort_fen n fen = let new_fen = bubble_sort_fen fen  in go_bubble_sort_fen (n-1) new_fen
bubble_sort_fen :: [Fenotype] -> [Fenotype]
bubble_sort_fen [] = []
bubble_sort_fen [x] = [x]
bubble_sort_fen ((Fen g1 f1 p1):(Fen g2 f2 p2):t)
    | f1 >= f2 = (Fen g2 f2 p2):(bubble_sort_fen ((Fen g1 f1 p1):t))
    | otherwise = (Fen g1 f1 p1):(bubble_sort_fen ((Fen g2 f2 p2):t))

-- проверка границы
border :: Fenotype -> Double -> Bool
border (Fen gen f p) bord
    | f >= bord = True 
    | otherwise = False

-- проверка качества
quality :: Fenotype -> Fenotype -> Bool 
quality (Fen _ f1 _) (Fen _ f2 _)
    | f1 > f2 = True
    | otherwise = False 

-- необходимое количество генов 
getNumberGen :: Double -> Double -> Double -> Int
getNumberGen down up delt = round $ (logBase 2.0 ((up - down)/delt)) + 0.49

-- коррекция шага
correction_step :: Int -> Double -> Double -> Double
correction_step n down up = (up - down)/(2**(fromIntegral n)-1) 