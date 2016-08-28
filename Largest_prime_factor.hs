intsqrt :: Int -> Int
intsqrt = floor . sqrt . fromIntegral

divizors x = [c | c <- [intsqrt x , intsqrt x - 1..2] , x`mod`c == 0]

prime :: Int -> Bool
prime n |length (divizors n) == 0 = True
        |otherwise = False

largest_prime_factor x = head [c | c <- [x`div`2,x`div`2 - 1..] , x `mod` c == 0, prime c == True]
