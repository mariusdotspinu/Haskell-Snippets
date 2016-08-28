--By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

--What is the 10 001st prime number?
intsqrt :: Int -> Int
intsqrt = floor . sqrt . fromIntegral

divizors x = [c | c <- [intsqrt x , intsqrt x - 1..2] , x`mod`c == 0]

prime :: Int -> Bool
prime n |length (divizors n) == 0 = True
        |otherwise = False

get_all_primes :: [Int] -> [Int] -> Int -> Int
get_all_primes my_list (x:xs) n
    | length my_list < n && prime x == True = get_all_primes (x : my_list) xs n
    | length my_list < n && prime x == False = get_all_primes my_list xs n
    | length my_list == n = head my_list


get_last_prime n = get_all_primes [] [2..] n
