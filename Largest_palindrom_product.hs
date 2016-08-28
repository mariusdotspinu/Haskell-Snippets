last_digit x = x `mod` 10

digit_zeros 0 = 1
digit_zeros x = 10 * digit_zeros (x `div` 10)

number_of_ten_to_multiply x = (digit_zeros x) `div` 10

max_n_number 1 = 9
max_n_number n = 10^(n) - 1

min_n_number 0 = 0
min_n_number n = 10^(n-1)



reverse' :: Int -> Int
reverse' x  = if x > 0
             then last_digit x * (number_of_ten_to_multiply x) +
                  reverse' (x `div` 10)
              else
                0

palindrom :: Int -> Bool
palindrom x | reverse' x == x = True
            | otherwise = False

product' :: Int -> Int -> Int
product' a b = a * b


--largest palindrome of two n-digit numbers
largest_palindrom a b n
            |palindrom (product' a b) == True = product' a b
            |b == 0 = largest_palindrom (a-1) (max_n_number n) n
            |palindrom (product' a b) == False = largest_palindrom a (b-1) n

l_palindrom n = largest_palindrom (max_n_number n) (max_n_number n) n
