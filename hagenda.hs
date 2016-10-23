type Height = Int
type Width = Int

data Picture = Picture {
    height :: Height,
    width :: Width,
    pixels :: [[Char]]
} deriving (Show)

 
x :: Picture
y :: Picture
a :: Picture
b :: Picture

(x,y,a,b) = (pixel 'x',pixel 'y',pixel 'a',pixel 'b')
pixel :: Char -> Picture
pixel c = Picture{
        height = 1,
        width = 1,
        pixels = [[c]]
        } 

above :: Picture -> Picture -> Picture
above p0 p1 
    | wp0 == wp1 = Picture {
                    height = hp0 + hp1,
                    width = wp0,
                    pixels = pp0 ++ pp1
                }
    |otherwise = error "error, difiere weight"
    where  (wp0, wp1) = (width p0, width p1)
           (hp0, hp1) = (height p0, height p1)
           (pp0, pp1) = (pixels p0, pixels p1) 

beside :: Picture -> Picture -> Picture
beside p0 p1
    | hp0 == hp1 = Picture {
                    height = hp0,
                    width = wp0 + wp1,
                    pixels =zipWith (++) pp0  pp1
                }
    |otherwise = error "error, difiere height"
    where  (wp0, wp1) = (width p0, width p1)
           (hp0, hp1) = (height p0, height p1)
           (pp0, pp1) = (pixels p0, pixels p1)

toString :: Picture -> String 
toString p  =  (foldr (++) "" (pixels p)

stack:: [Picture] -> Picture
stack = foldr1 above

--stack [x] = x
--stack (x:xs) = above x (stack xs) 

spread :: [Picture] -> Picture
spread = foldr1 beside

row :: String -> Picture
row  s = spread (map pixel s) 

blank :: (Height, Width) -> Picture
blank (h,w) = let stackB h = stack (replicate h (pixel ' '))
              in Picture {
                    height = h,
                    width = w,
                    pixels = pixels (spread (replicate w (stackB h)))
                }

stackWith :: Height -> [Picture] -> Picture
stackWith h ps = undefined