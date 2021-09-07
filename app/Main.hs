module Main where
import Data.List

import System.Random

type Vector = [Float]

type Matrix = [[Float]]

dotProduct :: Vector -> Vector -> Float
dotProduct v w = sum (zipWith (*) v w)

matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct m n = [map (dotProduct row) (transpose n) | row <- m]

arraySigmoid :: Vector -> Vector
arraySigmoid xs = [1 / (1 + exp (-x)) | x <- xs]

arraySigmoidDX :: Vector -> Vector
arraySigmoidDX vec = [x * (1 - x) | x <- vec]

matrixSigmoidDerivative :: Matrix -> Matrix
matrixSigmoidDerivative m = [arraySigmoidDX row | row <- m]

subtractMatrix :: Num c => [[c]] -> [[c]] -> [[c]]
subtractMatrix = zipWith (zipWith (-))

multiplyMatrix :: Num c => [[c]] -> [[c]] -> [[c]]
multiplyMatrix = zipWith (zipWith (*))

addMatrix :: Num c => [[c]] -> [[c]] -> [[c]]
addMatrix = zipWith (zipWith (+))

matrixSigmoid :: Matrix -> Matrix
matrixSigmoid m = [arraySigmoid row | row <- m]

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

think :: Matrix -> Matrix -> Matrix
think training_set_inputs starting_weights
  = matrixSigmoid (matrixProduct training_set_inputs starting_weights)

train :: Matrix -> Matrix -> Matrix -> Int -> Matrix
train starting_weights training_set_outputs training_set_inputs n
  = (iterate
       (\ weights ->
          (addMatrix
             (matrixProduct (transpose training_set_inputs)
                (multiplyMatrix
                   (subtractMatrix training_set_outputs
                      (think training_set_inputs weights))
                   (matrixSigmoidDerivative
                      (think training_set_inputs weights)))))

            weights)

       starting_weights)
      !! n

matrixVectorProduct :: Matrix -> Vector -> Vector
matrixVectorProduct m v = zipWith (*) v (concat m)

main
  = do x <- getSW 3
       y <- getSW 3
       let tsi = [[0, 0, 1], [1, 1, 1], [1, 0, 1], [0, 1, 1]]
       let tso = transpose [[0, 1, 1, 0]]
       let sw = [[0.1], [0.2], [0.3]]
       let pr = train sw tso tsi 20000
       let result = arraySigmoid [sum $ matrixVectorProduct pr [0, 1, 0]]
       putStrLn $ show $ pr
       putStrLn $ show $ result
