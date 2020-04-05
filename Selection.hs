module Selection where
import Common
import Random_
import Prelude 

-- выбор типа селекции
multiplexor_select :: Int -> Int -> [Fenotype] -> [Genotype]
multiplexor_select type_sel kol fen 
    | type_sel == 2 = stochastic_select 0.0 (1.0/(fromIntegral kol)) kol (get_probability fen (sum_fen fen) 0.0)
    | type_sel == 3 = tournament_select (length fen) kol fen
    | type_sel == 4 = rank_select kol fen
    | type_sel == 5 = random_select (length fen) kol fen
    | otherwise = selection kol (get_probability fen (sum_fen fen) 0.0)

-- селекция на турнире
tournament_select :: Int -> Int -> [Fenotype] -> [Genotype]
tournament_select _ 0 _ = []
tournament_select len n fen = new_gen:(tournament_select len (n-1) fen)
    where 
        new_gen = tournament (getFenNumb fen (random_int 0 (len-1)),
                          getFenNumb fen (random_int 0 (len-1)),
                          getFenNumb fen (random_int 0 (len-1)))

-- селекция стохастическая (разделение на секторы и выбор в них)
stochastic_select :: Double -> Double -> Int -> [Fenotype] -> [Genotype]
stochastic_select _ _ 0 _ = []
stochastic_select next sectors n a = (choice_gen (next+(random_double 0.0 sectors)) a): stochastic_select (next+sectors) sectors (n-1) a

-- селекция рандомная 
random_select :: Int -> Int -> [Fenotype] -> [Genotype]
random_select _ 0 _ = []
random_select len n fen = (getGenFromFen (getFenNumb fen (random_int 0 (len-1)))):(random_select len (n-1) fen)

-- ранговая селекция
rank_select :: Int -> [Fenotype] -> [Genotype]
rank_select kol fen = selection kol fen_ready 
    where 
        fen_rank = let fen_new = go_bubble_sort_fen (length fen) fen in (add_rank 1 fen_new)
        fen_ready = get_probability fen_rank (sum_fen fen_rank) 0.0

-- селекция классическая
selection :: Int -> [Fenotype] -> [Genotype]
selection 0 _ = []
selection n a = (choice_gen (random_double 0.0 1.0) a): selection (n-1) a

-- выбор гена для селекции
choice_gen :: Double -> [Fenotype] -> Genotype
choice_gen c ((Fen gen _ p):t)
    | c < p = gen 
    | otherwise = choice_gen c t 

-- обозначить вероятности для дальнейшей селекции
get_probability :: [Fenotype] -> Double -> Double -> [Fenotype]
get_probability [] _ _ = []
get_probability ((Fen a value p):t) sum cycle = 
    (Fen a value prob):(get_probability t sum prob)
        where 
            prob = cycle + _value / sum
            _value
                | and [value < 0, value > -1] = 1.0
                | value < 0 = 1/(- value)
                | otherwise = value

-- подсчет суммы фитнесс функции по всей популяции для выставления вероятностей
sum_fen :: [Fenotype] -> Double
sum_fen [] = 0
sum_fen ((Fen g fitness prob):t) = _value + (sum_fen t)
    where
        _value
            | and [fitness < 0, fitness > -1] = 1.0
            | fitness < 0 = 1/(- fitness)
            | otherwise = fitness

-- турнир
tournament :: (Fenotype, Fenotype, Fenotype) -> Genotype
tournament (Fen gen1 fitness1 _, Fen gen2 fitness2 _, Fen gen3 fitness3 _)
    | and [fitness1 >= fitness2, fitness1 >= fitness3] = gen1
    | and [fitness2 >= fitness1, fitness2 >= fitness3] = gen2 
    | otherwise = gen3

-- проставить ранг
add_rank :: Int -> [Fenotype] -> [Fenotype]
add_rank _ [] = []
add_rank rank ((Fen gen fit p):t) = (Fen gen (fromIntegral rank) p) : add_rank (rank+1) t
