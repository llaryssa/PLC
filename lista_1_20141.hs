import Data.Char
import Data.List


fatorial :: Int -> Int
fatorial 1 = 1
fatorial n = n * fatorial (n-1)

pascal :: Int -> Int -> Int
pascal n k = fatorial n`div` (fatorial k * fatorial (n-k))

bissexto :: Int -> Bool
bissexto n | (n `mod` 400) == 0                            = True
           | ((n `mod` 4) == 0) && ((n `mod` 100) /= 0)    = True
           | otherwise                                     = False
           
digits :: Int -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

pitagoras :: String -> Int
pitagoras s = if (length (digits m)) == 1 then m
              else compress (digits m)
              where m = compress (process s)

process :: String -> [Int]
process [] = []
process (s:sx) = if (ord s) > 96 then (((ord s) - 96) `mod` 9) : process sx
                 else                 (((ord s) - 64) `mod` 9) : process sx

compress :: [Int] -> Int
compress [] = 0
compress (x:xs) = if (x == 0) then 9 + compress xs
                  else             x + compress xs
                  
median :: [Int] -> Int
median list | (length_list `mod` 2) /= 0   = getCara sorted_list ((div length_list 2) + 1)
            | otherwise                    = div 
                                  ( (getCara sorted_list ((div length_list 2)    )) +
                                    (getCara sorted_list ((div length_list 2) + 1)) ) 2
            where         
              sorted_list = sort list
              length_list = length list 

getCara :: [Int] -> Int -> Int
getCara (x:xs) 1   = x
getCara (x:xs) idx = getCara xs (idx - 1) 

toInt :: [Integer] -> [Int]
toInt [] = []
toInt (x:xs) = (fromInteger x) : toInt xs

lista = toInt [3, 3, 3, 9, 11, 5]

aceita :: [Integer] -> [Integer] -> [(Integer, Integer, Integer)] -> Integer -> [Integer] -> Bool
-- aceita :: [a] -> [q] -> [(q, a, q)] -> q -> [q] -> Bool
-- // a entrada, conj de estados, transicoes, inicial e finais
aceita [] q t q0 qf = ehFinal q0 qf
aceita (x:xs) q t q0 qf = aceita xs q t (proximoEstado x q0 t) qf

ehFinal :: Integer -> [Integer] -> Bool
ehFinal x [] = False
ehFinal x (y:ys) | (x == y)  = True
                 | otherwise = ehFinal x ys
                 
proximoEstado :: Integer -> Integer -> [(Integer, Integer, Integer)] -> Integer
proximoEstado x q0 ((q1, a, q2):ts) | (q0 == q1) && (x == a) = q2
                                    | otherwise              = proximoEstado x q0 ts

s = [1, 0, 0, 1]
q = [1, 2, 3, 4]
q0 = 1
qf = [4]
t = [(1, 1, 1),(1, 0, 2),(2, 1, 1),(2, 0, 3),(3, 1, 4),(3, 0, 3),(4, 1, 4),(4, 0, 4)]


s0 = [1, 1, 1, 1, 1, 1]
s1 = [1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0]
s2 = [1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0]
s3 = [1, 1, 1, 1]
s4 = []


r = 
 [aceita s0 q t q0 qf,
  aceita s1 q t q0 qf,
  aceita s2 q t q0 qf,
  aceita s3 q t q0 qf,
  aceita s4 q t q0 qf]















