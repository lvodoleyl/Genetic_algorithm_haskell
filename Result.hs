module Result where
import Common
import Prelude

-- сформировать строку для записи в файл
form_string :: Optimize -> [Fenotype] -> String
form_string _ [] = "\n"
form_string (Opt down up delta func) ((Fen (Gen bin dec) fitness p):t) = 
    (show (down+((fromIntegral dec)*delta)))++";"++(show fitness)++" "++form_string (Opt down up delta func) t

result_string :: Optimize -> Fenotype -> String
result_string (Opt down up d f) (Fen (Gen bin dec) fit p) =
    "Result: "++(show (down+((fromIntegral dec)*d)))++";"++(show fit)

write_to_file :: String -> IO()
write_to_file str = appendFile "genetic_alg.txt" str

clear_file :: IO()
clear_file = writeFile "genetic_alg.txt" ""

str_int :: String -> Int 
str_int s = read s::Int 

str_double :: String -> Double 
str_double s = read s::Double 
