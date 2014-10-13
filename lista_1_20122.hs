digits :: Int -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

sort :: (a->a->Bool) -> [a] -> [a]
sort f []     = []
sort f (a:ax) = (sort f [b | b <- ax, not (f a b)]) 
                            ++ [a] ++ 
                (sort f [b | b <- ax,     (f a b)])


function :: Int -> Int -> Bool
function a b = sum (digits a) < sum (digits b)

ordenar :: [Int] -> [Int]
ordenar [] = []
ordenar a = sort function a


agrupar :: Eq a => [a] -> [[a]]
agrupar []     = []
agrupar (a:as) = [a : [b | b <- as, b == a]] ++ agrupar (remove a as)

remove :: Eq a => a -> [a] -> [a]
remove a [] = []
remove a (b:bs) = if (a == b) then remove a bs else (b:remove a bs)

divide :: (a -> Bool) -> [a] -> ([a],[a])
divide f [] = ([],[])
divide f a = ([b | b <- a, not (f b)] , [b | b <- a, (f b)])