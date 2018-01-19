-- 1:
add :: Integer -> Integer -> Integer
add 0 = id
add x | x > 0 = add (x - 1) . (+ 1)
      | x < 0 = add (x + 1) . (subtract 1)

-- 2:
mult :: Integer -> Integer -> Integer
mult x y | x == 0 = 0
         | x > 0 = helperMult x y y
         | x < 0 && y > 0 = helperMult y x x
         | otherwise = helperMult (negate x) (negate y) (negate y)
helperMult :: Integer -> Integer -> Integer -> Integer
helperMult x acc y | x == 1 = acc
               | x > 0 = helperMult (x - 1) (acc + y) y

-- 3:
sub :: Integer -> Integer -> Integer
sub x y = add x (negate y)

-- 4:
divhw :: Integer -> Integer -> Integer
divhw x y | mult x y > 0 = helperDiv (abs x) 0 (abs y)
          | otherwise = negate $ helperDiv (abs x) 0 (abs y)
helperDiv :: Integer -> Integer -> Integer -> Integer
helperDiv x acc y | x >= y = helperDiv (x - y) (acc + 1) y
                  | otherwise = acc

-- 5:
modhw :: Integer -> Integer -> Integer
modhw x y = sub x (mult y $ divhw x y)

-- 6:
countSumDiv :: Integer -> (Integer, Integer)
countSumDiv n = iterate 2 2 (n + 1) where
        n' = abs n
        iterate i cnt sm | i * i > n' = (,) cnt sm
                         | otherwise = if modhw n' i == 0 then iterate (i + 1) (cnt + 2) (sm + i + (divhw n i))
                                                          else iterate (i + 1) cnt sm

-- 7:
gcdhw :: Integer -> Integer -> Integer
gcdhw a b | b == 0 = a
          | otherwise = gcdhw b (modhw a b)

-- 8:
phihw :: Integer -> Integer
phihw n = iterate 2 n' n' where
        n' = abs n
        helperPhi x y = if modhw x y == 0 then helperPhi (divhw x y) y else x
        iterate i result x | i * i > x = if (x > 1) then result - (divhw result x) else result
                           | otherwise = if modhw x i == 0 then iterate (i + 1) (divhw result i) (helperPhi x i)
                                                           else iterate (i + 1) result x

-- M. Simuni

-- 1:
f :: Integer -> Float
f n | n == 0 = 1
    | otherwise = 1 + 1 / (f (n - 1))

-- 2:
b :: Integer -> Float
helperB :: Integer -> Float -> Float
b n = helperB n 0
helperB n i | n == 0 = i
            | otherwise = i + 1 / (helperB (n - 1) (i + 1))

-- 3:
sumsin :: Float -> Float
helperSumsin :: Float -> Float -> Float
sumsin n = sin (n * (n + 1) / 2) / helperSumsin n 0
helperSumsin n acc | n == 0 = acc
                   | otherwise = helperSumsin (n - 1) (acc + sin n)

-- 4:
sumfact :: Integer -> Integer
helperSumfact :: Integer -> Integer -> Integer
sumfact n = helperSumfact n 1
helperSumfact n acc | n == 0 = acc - 1
                    | otherwise = helperSumfact (n - 1) (acc * n + 1)

-- 5:
nseq :: Integer -> Integer
recur :: Integer -> Integer -> Integer -> Integer
loopNseq :: Integer -> Integer -> Integer -> Integer -> Integer
nseq n = recur 1 n 0
recur i n acc | i == n = acc + 1
              | otherwise = recur (i + 1) n (acc + loopNseq i (i + 1) n 0)
loopNseq from cur n acc | from + cur > n = acc
                        | otherwise = if ((from + cur) == n)
                                        then loopNseq from (cur + 1) n (acc + 1)
                                        else loopNseq from (cur + 1) n (acc + loopNseq (from + cur) (cur + 1) n 0)

-- 6:
g :: Integer -> Bool
g n = loop 2 (n - 2) where
      loop a b | a >= b = False
               | otherwise = if countSumDiv a == (2, a + 1) && countSumDiv b == (2, b + 1)
                              then True
                              else loop (a + 1) (b - 1)

-- 7:
c :: Integer -> [[Integer]]
c n = 

-- Зависимые типы

-- 1:
