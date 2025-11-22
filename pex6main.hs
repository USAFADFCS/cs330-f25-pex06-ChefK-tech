-- pex6.hs 
-- unKnot Haskell

-- name: Jeff Kwon

{- DOCUMENTATION:
https://wiki.haskell.org/How_to_work_on_lists
Used the above website as a resource to see the different list functions
I could use for this Pex. I used (last) and (init)to easily access the last element in the tripCode
as that is necessary to check considering the first and last pairs of the tripCode are adjacent.
-}

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "not a knot"
   | typeICheck tripCode = unKnot (typeIknot tripCode)
   | typeIICheck tripCode 1 = unKnot (typeIIknot tripCode 1)
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

typeICheck :: [(Char, Char)] -> Bool
typeICheck [_] = False
typeICheck (a:b:xs)
   | null (xs) = False
   | fst (a) == fst (b) = True
   | otherwise = typeICheck (b:xs)

typeIknot :: [(Char, Char)] -> [(Char, Char)]
typeIknot (a:b:xs)
   | null (xs) = xs
   | null [] = []
   | fst (a) == fst b = xs
   | otherwise = typeIknot (b:xs)

typeIICheck :: [(Char, Char)] -> Int -> Bool
typeIICheck _ _ = False
typeIICheck (h:t:xs) a
   | null (t:xs) = False
   | a == 1 =
        if snd h == snd (last (t:xs))
        then typeIIPairCheck (fst h) (fst (last (t:xs))) (init (t:xs))
        else typeIICheck (t:xs) 0
   | snd h == snd t =
        typeIIPairCheck (fst h) (fst t) xs
   | otherwise =
        typeIICheck (t:xs) 0

typeIIknot :: [(Char, Char)] -> Int -> [(Char, Char)]
typeIIknot xs _ = xs
typeIIknot (h:t:xs) a
   | a == 1 =
        if snd h == snd (last (t:xs))
        then if typeIIPairCheck (fst h) (fst (last (t:xs))) (init (t:xs))
             then drop 1 (typeIIPair (fst h) (fst (last (t:xs))) (init (t:xs)))
             else h : typeIIknot (t:xs) 0
        else h : typeIIknot (t:xs) 0
   | snd h == snd t =
        if typeIIPairCheck (fst h) (fst t) xs
        then drop 1 (typeIIPair (fst h) (fst t) xs)
        else h : typeIIknot (t:xs) 0
   | otherwise =
        h : typeIIknot (t:xs) 0

typeIIPair :: Char -> Char -> [(Char,Char)] -> [(Char,Char)]
typeIIPair _ _ xs = xs
typeIIPair _ _ [_] = []
typeIIPair a b (x:y:xs)
   | snd x == snd y &&
     ((a == fst x && b == fst y) || (a == fst y && b == fst x)) =
         xs
   | otherwise = x : typeIIPair a b (y:xs)

typeIIPairCheck :: Char -> Char -> [(Char,Char)] -> Bool
typeIIPairCheck _ _ [_] = False
typeIIPairCheck a b (x:y:xs)
   | snd x == snd y &&
     ((a == fst x && b == fst y) || (a == fst y && b == fst x)) =
         True
   | otherwise =
         typeIIPairCheck a b (y:xs)

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

