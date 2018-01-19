-- 1:
minlist :: Ord a => [a] -> a
minlist lst = iterate (head lst) lst where
    iterate ans [] = ans
    iterate ans (l:ls) = iterate (if ans < l then ans else l) ls

-- 2:
sumprod :: Num a => [a] -> a
sumprod (l1:(l2:[])) = l1 * l2
sumprod (l1:(l2:ls)) = l1 * l2 + sumprod (l2 : ls)

-- 3:
check :: (a -> Bool) -> [a] -> Bool
check _ [] = False
check cond (x:xs) = if cond x == True then True else check cond xs

-- 4:
sameDigits :: [Int] -> Bool
sameDigits [] = False
sameDigits lst = length lst > 10 || check (map (\x -> x `mod` 10) lst) where
    find f [] = Nothing
    find f (l:ls) = if f l == True then Just l else find f ls
    check (l:ls) = if find (\x -> x == l) ls /= Nothing then True else sameDigits ls

-- 5:
upDown :: Ord a => [a] -> Bool
upDown [] = False
upDown (l1:(l2:ls)) = if l2 < l1 then False else iterate (l2:ls) False where
    iterate [] state = state
    iterate (l:[]) state = state
    iterate (l1:(l2:ls)) state = if l2 > l1 && state == True then False
        else if l2 < l1 && state == False then iterate (l2:ls) True
        else iterate (l2:ls) state

-- 6:
parts :: Ord a => [a] -> Bool
parts lst = check 2 lst where
    check n lst | n > length lst = False
                | length lst `mod` n /= 0 = check (n + 1) lst
                | otherwise = if countUp 0 (divide lst n []) == length lst `div` n then True else check (n + 1) lst
    divide [] n res = res
    divide (x:xs) n [] = divide xs n [[x]]
    divide (x:xs) n (c:cs) = if length c == n then divide xs n ([x] : (c:cs)) else divide xs n ((c ++ [x]):cs)
    countUp res [] = res
    countUp res (x:xs) = if isUp x then countUp (res + 1) xs else countUp res xs
    isUp (l1:(l2:[])) = l2 > l1 
    isUp (l1:(l2:ls)) = if l2 > l1 then isUp (l2:ls) else False

-- 7:
parts2 :: Ord a => [a] -> Bool
parts2 lst = 