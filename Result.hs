module Result where
import Common
import Prelude

-- сформировать строку для записи в файл
form_string :: Optimize -> [Fenotype] -> String
form_string _ [] = "\n"
form_string (Opt down up delta func) ((Fen (Gen bin dec) fitness p):t) = 
    (show (down+((fromIntegral dec)*delta)))++";"++(show fitness)++" "++form_string (Opt down up delta func) t

write_to_file :: String -> IO()
write_to_file str = appendFile "genetic_alg.txt" str

clear_file :: IO()
clear_file = writeFile "genetic_alg.txt" ""