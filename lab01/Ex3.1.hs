-- | To define the collatz function
collatz :: Integer->Integer
collatz n 
        | n==1 =1
        | even n = 1+ collatz (div n 2)
        | otherwise  = 1+ collatz (3*n+1)

collatz' :: Integer->Integer
collatz' n= if n==1 
            then 1
            else 
                if even n 
                then 1 + collatz'(div n 2)
                else 1 + collatz'(3*n +1)            

