module GENETIC where
import Mutation 
import Selection 
import Crossingover
import Random_ 
import Common
import Prelude

-- основная итерация цикла
result :: Parametrs -> [Fenotype] -> Optimize -> [Fenotype]
result (P popul sel c c_n c_p m_p s s_n) f o = f_res
    where
        g_new = multiplexor_select sel popul f
        g_cross = multiplexor_cross c c_n c_p g_new
        g_mut = mutation m_p g_cross
        f_res = list_fen g_mut o

-- по количеству итераций
go_iter :: Parametrs -> [Fenotype] -> Optimize -> [Fenotype]
go_iter (P popul _ _ _ _ _ _ 0) fenot _ = fenot
go_iter (P popul sel c c_n c_p m_p s s_n) fenot optim = go_iter (P popul sel c c_n c_p m_p s (s_n-1)) fenot_new optim
    where
        fenot_new = result (P popul sel c c_n c_p m_p s s_n) fenot optim

-- по поиску минимальной границы
go_minimal :: Parametrs -> [Fenotype] -> Optimize -> Int -> [Fenotype]
go_minimal (P popul sel c c_n c_p m_p s s_n) fenot optim protect
    | or [border (max_fen fenot (Fen (Gen [0] 0) (-1000000.0) 0)) (fromIntegral s_n), protect==0] = fenot
    | otherwise = go_minimal (P popul sel c c_n c_p m_p s s_n) fenot_new optim (protect-1)
        where
            fenot_new = result (P popul sel c c_n c_p m_p s s_n) fenot optim

-- по не изменению качества
go_quality :: Parametrs -> [Fenotype] -> Optimize -> Int -> Fenotype -> [Fenotype]
go_quality (P popul sel c c_n c_p m_p s s_n) fenot optim cycle prev
    | cycle == s_n = fenot
    | quality (max_fen fenot (Fen (Gen [0] 0) (-1000000.0) 0)) prev = go_quality (P popul sel c c_n c_p m_p s s_n) fenot_new optim 0 (max_fen fenot (Fen (Gen [0] 0) (-1000000.0) 0))
    | otherwise = go_quality (P popul sel c c_n c_p m_p s s_n) fenot_new optim (cycle+1) prev
        where
            fenot_new = result (P popul sel c c_n c_p m_p s s_n) fenot optim

-- выбор способа завершения
multiplexor_end :: Parametrs -> Optimize -> [Fenotype] -> [Fenotype]
multiplexor_end (P popul sel c c_n c_p m_p s s_n) o fen 
    | s == 1 = go_quality (P popul sel c c_n c_p m_p s s_n) fen o 0 (max_fen fen (Fen (Gen [0] 0) (-1000000.0) 0))
    | s == 3 = go_minimal (P popul sel c c_n c_p m_p s s_n) fen o 1000000
    | otherwise = go_iter (P popul sel c c_n c_p m_p s s_n) fen o

main::IO()
main = do
    p1 <- return $ (\x -> 3*sin(x*0.1)*x) -- функция
    p2 <- return $ 0.0 --нижняя граница
    p3 <- return $ 31.0 --верхняя граница
    p4 <- return $ 1.0 --погрешность или шаг
    p5 <- return $ 30 --количество особей в популяции
    p6 <- return $ 3 --тип селекции 1 - рулетка, 2 - стохастика, 3 -турнир, 4 - ранг, 5 - сл.в.
    p7 <- return $ 1 --тип кроссовера 1 - многоточ, 2 - единый
    p8 <- return $ 1 --количество точек кроссинговера (1 - одноточечный)
    p9 <- return $ 0.5 --вероятность кроссинговера
    p10 <-return $ 0.1 --вероятность мутации
    p11 <-return $ 1 --критерий остановки 1 - не улучшается N популяций, 2 - лимит популяции, 3 - минимальный прикол
    p12 <-return $ 40 --либо популяций ,либо минимальное значение фитнесс-функции
    p <- return $ P p5 p6 p7 p8 p9 p10 p11 p12
    o <- return $ Opt p2 p3 p4 p1
    g <- return $ init_gens p5 5 --надо высчитывать этот шаг и сколько генов надо
    f <- return $ multiplexor_end p o (list_fen g o) 
    print $ max_fen f (Fen (Gen [0] 0) (-1000000.0) 0)