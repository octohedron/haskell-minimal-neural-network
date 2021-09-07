module Main where
import Data.List
import Numeric

import System.Random

type Vector = [Float]

type Matrix = [[Float]]

getRF :: StdGen -> (Float, StdGen)
getRF gen = randomR (-1.0, 1.0) gen

getW :: Int -> IO ([Float])
getW 0 = return []
getW n
  = do gen <- newStdGen
       let p = fst $ getRF gen
       ps <- getW (n - 1)
       return (p : ps)

getSW :: Int -> IO ([[Float]])
getSW 0 = return []
getSW n
  = do p <- getW 1
       ps <- getSW (n - 1)
       return (p : ps)

t = transpose
z = zipWith
as a = [1 / (1 + exp (-b)) | b <- a]
ms m = [as r | r <- m]
ax v = [x * (1 - x) | x <- v]
mx m = [ax r | r <- m]
sm = z (z (-))
ml = z (z (*))
ad = z (z (+))
dp v w = sum (z (*) v w)
mp m n = [map (dp r) (t n) | r <- m]
g t w = ms (mp t w)
tr a b c n
  = (iterate (\ x -> (ad (mp (t c) (ml (sm b (g c x)) (mx (g c x))))) x) a) !! n

mvml :: Matrix -> Vector -> Vector
mvml m v = zipWith (*) v (concat m)

main
  = do let ti = [[0, 0, 1], [1, 1, 1], [1, 0, 1], [0, 1, 1]]
       let o = t [[0, 1, 1, 0]]
       let s = [[0.1], [0.2], [0.3]]
       let pr = tr s o ti 60
       let x = t pr
       let r = as [sum $ mvml x [0, 1, 0]]
       putStrLn $ show $ pr
       putStrLn $ show $ r
