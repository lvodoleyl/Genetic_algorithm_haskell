import Mutation 
import Selection 
import Crossingover
import Random_ 
import Common
import Prelude
import Func_opt
import Result
import System.Environment
import System.IO.Unsafe

-- основная итерация цикла
result :: Parametrs -> [Fenotype] -> Optimize -> [Fenotype]
result (P popul sel c c_n c_p m_p s s_n) f o = f_res
    where
        g_new = multiplexor_select sel popul f
        g_cross = multiplexor_cross c c_n c_p g_new
        g_mut = mutation m_p g_cross
        f_res = list_fen g_mut o

-- по количеству итераций
go_iter_io :: Parametrs -> [Fenotype] -> Optimize -> IO ([Fenotype])
go_iter_io (P popul sel c c_n c_p m_p s s_n) fenot optim = case (s_n == 0) of 
    True -> return fenot
    False -> do
        fenot_new <- return $ result (P popul sel c c_n c_p m_p s s_n) fenot optim
        write_to_file (form_string optim fenot_new)
        go_iter_io (P popul sel c c_n c_p m_p s (s_n-1)) fenot_new optim
-- по поиску минимальной границы
go_minimal_io :: Parametrs -> [Fenotype] -> Optimize -> Int -> IO ([Fenotype])
go_minimal_io (P popul sel c c_n c_p m_p s s_n) fenot optim protect = case (or [border (max_fen fenot (Fen (Gen [0] 0) (-1000000.0) 0)) (fromIntegral s_n), protect==0]) of
    True -> return fenot
    False -> do
        fenot_new <- return $ result (P popul sel c c_n c_p m_p s s_n) fenot optim
        write_to_file (form_string optim fenot_new)
        go_minimal_io (P popul sel c c_n c_p m_p s s_n) fenot_new optim (protect-1)
-- по не изменению качества
go_quality_io :: Parametrs -> [Fenotype] -> Optimize -> Int -> Fenotype -> IO([Fenotype])
go_quality_io (P popul sel c c_n c_p m_p s s_n) fenot optim cycle prev = case (cycle == s_n) of
    True -> return fenot 
    False -> do
        fenot_new <- return $ result (P popul sel c c_n c_p m_p s s_n) fenot optim
        write_to_file (form_string optim fenot_new)
        if (quality (max_fen fenot (Fen (Gen [0] 0) (-1000000.0) 0)) prev)
            then go_quality_io (P popul sel c c_n c_p m_p s s_n) fenot_new optim 0 (max_fen fenot (Fen (Gen [0] 0) (-1000000.0) 0))
            else go_quality_io (P popul sel c c_n c_p m_p s s_n) fenot_new optim (cycle+1) prev
-- выбор способа завершения
multiplexor_end :: Parametrs -> Optimize -> [Fenotype] -> IO([Fenotype])
multiplexor_end (P popul sel c c_n c_p m_p s s_n) o fen 
    | s == 1 = go_quality_io (P popul sel c c_n c_p m_p s s_n) fen o 0 (max_fen fen (Fen (Gen [0] 0) (-1000000.0) 0))
    | s == 3 = go_minimal_io (P popul sel c c_n c_p m_p s s_n) fen o 10000
    | otherwise = go_iter_io (P popul sel c c_n c_p m_p s s_n) fen o

main::IO()
main = do
    args <- getArgs
    clear_file
    p1 <- return $ func_opt -- функция
    p2 <- return $ str_double $ args !! 0 --нижняя граница
    p3 <- return $ str_double $ args !! 1 --верхняя граница
    p4 <- return $ str_double $ args !! 2 --погрешность или шаг
    p5 <- return $ str_int $ args !! 3 --количество особей в популяции
    p6 <- return $ str_int $ args !! 4 --тип селекции 1 - рулетка, 2 - стохастика, 3 -турнир, 4 - ранг, 5 - сл.в.
    p7 <- return $ str_int $ args !! 5 --тип кроссовера 1 - многоточ, 2 - единый
    p8 <- return $ str_int $ args !! 6 --количество точек кроссинговера (1 - одноточечный)
    p9 <- return $ str_double $ args !! 7 --вероятность кроссинговера
    p10 <-return $ str_double $ args !! 8 --вероятность мутации
    p11 <-return $ str_int $ args !! 9 --критерий остановки 1 - не улучшается N популяций, 2 - лимит популяции, 3 - минимальный прикол
    p12 <-return $ str_int $ args !! 10 --либо популяций ,либо минимальное значение фитнесс-функции
    p <- return $ P p5 p6 p7 p8 p9 p10 p11 p12
    n_gen <- return $ (getNumberGen p2 p3 p4)
    g <- return $ init_gens p5 n_gen
    o <- return $ Opt p2 p3 (correction_step n_gen p2 p3) p1
    f <- multiplexor_end p o (list_fen g o)
    res <- return $ max_fen f (Fen (Gen [0] 0) (-1000000.0) 0)
    print $ res
    write_to_file (result_string o res)
    return()
