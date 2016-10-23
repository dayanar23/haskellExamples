{-	

-}

--UNZIP RECURSION DIRECTA
unzipR [] = ([],[])
unzipR ((a,b):xs) = (a:as,b:bs)
	where (as,bs) = unzipR xs


--UNZIP FOLDR 
unzipF:: [(a,b)] -> ([a],[b])
unzipF = foldr conss ([],[])
	where conss (x,y)(xs,ys) = (x:xs,y:ys)

--UNZIP MAP

unzipM:: [(a,b)] -> ([a],[b])
unzipM = par (map fst, map snd)
	where par (f,g) x = (f x ,g x)

--UNZIP COMPRENSION LIST

unzipL:: [(a,b)] -> ([a],[b])
unzipL x = head[([xs],[ys])| (xs,ys) <- x]

