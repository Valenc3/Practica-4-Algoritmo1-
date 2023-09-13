
--Ejercicio 1

fibonacci:: Integer -> Integer
fibonacci x| x == 0 = 0
           | x == 1 = 1
           | otherwise = fibonacci (x-1) + fibonacci (x-2)
--Ejercicio 2

parteNOentera :: Float ->Float
parteNOentera x| x < 0 = x + 1
               | x == 0 = 0
               | x > 0 = parteNOentera (x-1)


parteEntera :: Float -> Int
parteEntera x| 0 < x && x < 1 = 0
             | otherwise = 1 + parteEntera (x-1)

--Ejercicio 3

esDivisible :: Integer ->Integer ->Bool
esDivisible x y| x < y = False
               | x - y == 0 = True
               | otherwise = esDivisible (x-y) y

--Ejercicio 4

sumaImpares :: Integer ->Integer
sumaImpares x| x == 1 = 1
             | otherwise = impares x + sumaImpares (x-1)


impares :: Integer ->Integer
impares x| x == 1 = 1
         | otherwise = 2 + impares (x-1)


--Ejercicio 5 

medioFact :: Integer ->Integer
medioFact x| x == 1 || x == 0 = 1
           | otherwise = x*medioFact (x-2)


--Ejercicio 6

sumaDigitos :: Integer ->Integer
sumaDigitos x| x < 10 = x
             | otherwise = mod x 10 + sumaDigitos (menosdigito x)

--Ejercicio 7

menosdigito :: Integer -> Integer
menosdigito x = div x 10

todosDigitosIguales :: Integer -> Bool
todosDigitosIguales x| x < 10 = True
                     | mod x 10*11 == mod x 100 = todosDigitosIguales (menosdigito x)
                      | otherwise = False


--Ejercicio 8

iesimoDigito :: Integer ->Integer ->Integer
iesimoDigito x y| y == cantDigitos x = mod x 10
                | otherwise = iesimoDigito (menosdigito x) y

cantDigitos :: Integer -> Integer
cantDigitos x| x < 10 = 1
             | otherwise = 1 + cantDigitos (menosdigito x)


--Ejercicio 9 

esCapicua :: Integer -> Bool
esCapicua x| x < 10 = True
           | x < 100 && mod x 10 == div x 10 = True
           | mod x 10 == div x (10^((cantDigitos x)-1)) = esCapicua ((div x 10) -(mod x 10)*(10^((cantDigitos x)-2)))
           |otherwise = False


--Ejercicio 10 

--a
f1 :: Integer -> Integer
f1 x| x == 0 = 1
    | otherwise = 2^x + f1 (x-1)
--f1 x = 2^(x+1)-1


--b
--f2 :: Integer -> Integer -> Integer
--f2 x y| x == 0 = 1 
--      | otherwise = y ^ x + f2 (x-1) y

--c
f3 :: Integer -> Integer -> Integer
f3 x y| x == 0 = 0
      | otherwise = y ^ (x*2 - 1) + y ^ (x*2) + f3 (x-1) y


--d
f4 :: Integer -> Integer -> Integer
f4 x y = (f3 x y) - (f2 x y) + y ^ x


--Ejercicio 11
eAprox :: Integer -> Float
eAprox x| x == 0 = 1.0
        | otherwise = 1.0 / fromIntegral (factorial x) + eAprox (x-1)

factorial :: Integer -> Integer
factorial x| x == 0 = 1
           | otherwise = x*(factorial (x-1))

e = eAprox 10

--Ejercicio 12

raizDe2Aprox :: Int -> Float
raizDe2Aprox x = aN x - 1

aN :: Int -> Float
aN x| x == 1 = 2
    | otherwise = 2 + 1/(aN (x-1))


--Ejercicio 13 

sumaDoble :: Integer -> Integer -> Integer
sumaDoble x y| y == 0 = 0
             | otherwise = f2 x y + sumaDoble x (y-1)

f2 :: Integer -> Integer -> Integer
f2 x y| x == 1 = y
      | otherwise = y ^ x + f2 (x-1) y
--Ejercicio 14



sumaAuxiliar :: Integer -> Integer -> Integer -> Integer
sumaAuxiliar x y z| y == 0 && x == 0 = 1
                  | x == 0 = z^y +sumaAuxiliar x (y-1) z
                  | y == 0 = z^x +sumaAuxiliar (x-1) y z
                  | otherwise = z^(x+y) + sumaAuxiliar (x-1) y z
{-
Esta cuenta los casos como x = 2 && y = 1 con y = 2 && x = 1 diferentes 
sumaPotencias :: Integer ->Integer ->Integer ->Integer
sumaPotencias x y z| x == 0 && y == 0 = 1
                   | otherwise = sumaAuxiliar x y z + sumaPotencias x (y-1) z 


sumaAuxiliar :: Integer -> Integer -> Integer -> Integer 
sumaAuxiliar x y z| x == 0 = z^y
                  | otherwise = z^(x+y) + sumaAuxiliar (x-1) y z 

-}
--Ejercicio 15 

sumaRacionales :: Integer -> Integer ->Float
sumaRacionales x y| y == 0 = 0
                  | otherwise = funcionAuxiliar2 x y + sumaRacionales x (y-1)

funcionAuxiliar2 :: Integer -> Integer -> Float
funcionAuxiliar2 x y| x == 1 = fromInteger (x)/fromInteger (y)
                    | otherwise = fromInteger (x)/fromInteger (y) + funcionAuxiliar2 (x-1) y


--Ejercicio 16 

--a 
menorDivisor :: Integer ->Integer
menorDivisor x| x == 1 = 1
              | otherwise = divisionLista [1..x] x

divisionLista :: [Integer] -> Integer -> Integer
divisionLista [] n = 0
divisionLista (1:xs) n  = divisionLista xs n
divisionLista (x:xs) n
  | mod n x == 0 = x
  | otherwise = divisionLista xs n


estanRelacionados :: Integer  -> Integer  -> Bool
estanRelacionados x y| x == 0 || y == 0 = False
                     | mod x y == 0 = True
                     | otherwise = False

{-sumaRacionales2 :: Integer -> Integer -> Float 
sumaRacionales2 x y| y == 0 = 0
                   | x /= 0 =  fromInteger(x)/fromInteger(y) + sumaRacionales2 (x-1) y
                   | otherwise =  fromInteger(x)/fromInteger(y+1) + sumaRacionales2 x (y-1) -}

--b 
esPrimo :: Integer -> Bool
esPrimo x| x == 1 = False
         |menorDivisor x == x = True
         | otherwise = False


--c
--sonCoprimos :: Integer -> Integer -> Bool
--sonCoprimos a b = (mcd a b) == 1

sonCoprimos :: Integer ->Integer ->Bool
sonCoprimos x y = auxiliarCoprimos (listaDivisores x) (listaDivisores y) 

auxiliarCoprimos :: [Integer] -> [Integer] -> Bool
auxiliarCoprimos [] (y:ys) = True 
auxiliarCoprimos (x:xs) (y:ys)| mod y x == 0 = False 
                              | otherwise = auxiliarCoprimos (xs) (y:ys)

listaDivisores :: Integer -> [Integer]
listaDivisores x| x == 1 = []
                | menorDivisor x == menorDivisor (div x (menorDivisor x)) = listaDivisores (div x (menorDivisor x))
                | otherwise = menorDivisor x : listaDivisores (div x (menorDivisor x))
--d
eNesimoPrimo :: Integer -> Integer 
eNesimoPrimo x = auxiliar x 1


auxiliar :: Integer -> Integer -> Integer 
auxiliar x y| x == 1 = 2
            | esPrimo(eNesimoPrimo (x-1) + y) = eNesimoPrimo(x-1) + y
            | otherwise = auxiliar x (y+1)


--Ejercicio 17

esFibonacci :: Integer ->Bool
esFibonacci x = esFibonacci1 x 1

esFibonacci1 :: Integer -> Integer -> Bool
esFibonacci1 x y| x == fibonacci y = True 
                | x > fibonacci y = False 
                | otherwise = esFibonacci1 x (y+1)

--Ejercicio 18 

mayorDigitoPar :: Integer -> Integer 
mayorDigitoPar x| x == 0 = -1
                | even x && x<10 = x
                | mod x 10 == 0 || odd (mod x 10) = mayorDigitoPar (div x 10)
                | otherwise = max (mod x 10) (mayorDigitoPar (div x 10))


--Ejercicio 19 

esSumaInicialDePrimos :: Integer ->Bool
esSumaInicialDePrimos x = esSumaInicialDePrimos1 x 1


esSumaInicialDePrimos1 :: Integer -> Integer ->Bool
esSumaInicialDePrimos1 x y| x == sumaInicialDePrimos y = True 
                         | x > sumaInicialDePrimos y = False 
                         | otherwise = esSumaInicialDePrimos1 x (y+1)


sumaInicialDePrimos :: Integer -> Integer 
sumaInicialDePrimos x| x == 1 = 2
                     | otherwise = eNesimoPrimo x + sumaInicialDePrimos (x-1)
                       
--Ejercicio 20 
