module Main where
import Data.Bits
import Data.List (zipWith5, zipWith4)
import Data.Bifunctor
import Debug.Trace
import Distribution.Compat.Prelude (readMaybe)


type Mat = [Vec]
type Vec = [Float]

main  :: IO ()
main = mnist

fileIO :: IO ()
fileIO = do
    filepath <- getLine
    file <- readFile filepath
    print $ (shaper . floater . split) file
    --just print matrix 

split :: String -> [String]
split str
    | Prelude.null str = []
    | otherwise = takeWhile (/=',') str : split (if Prelude.null (dropWhile (/=',') str) then [] else tail (dropWhile (/=',') str))

oneHot :: Int -> Int -> Vec
oneHot idx max = take (max+1) $ map (fromIntegral . (\i -> if i == idx then 1 else 0)) [0..]

floater :: [String] -> [Float]
floater (s:trList) = case (readMaybe::String -> Maybe Float) s of
                    Just num -> num : floater trList
                    Nothing -> error "userfault"

shaper :: [Float] -> [(Vec, Vec)]
shaper (f:fs) = ("code : " ++ show f) `trace` (oneHot 10 $ round f, take 784 fs) : shaper (drop 784 fs)
shaper fs = []
--todo: this may wrong

strassen :: Mat -> Mat -> Mat
strassen as_ bs_ =
    let as = as_ in
    let bs =bs_ in
        if length as == 2 && length bs == 2
            then
                let a_one_one = map (fst. vecSplit) $ (fst. matSplit) as in
                let a_one_two = map (snd . vecSplit) $ (fst. matSplit) as in
                let a_two_one = map (fst. vecSplit) $ (snd. matSplit) as in
                let a_two_two = map (snd . vecSplit) $ (snd. matSplit) as in
                let b_one_one = map (fst. vecSplit) $ (fst. matSplit) bs in
                let b_one_two = map (fst . vecSplit) $ (snd. matSplit)  bs in
                let b_two_one = map (snd. vecSplit) $ (fst. matSplit) bs in
                let b_two_two = map (snd . vecSplit) $ (snd. matSplit) bs in

                    let m1 = (a_one_one `matAdd` a_two_two) `matMulComp` (b_one_one `matAdd` b_two_two) in
                    let m2 = (a_two_one `matAdd` a_two_two) `matMulComp` b_one_one in
                    let m3 = a_one_one `matMulComp` (b_one_two `matSub` b_two_two) in
                    let m4 = a_two_two `matMulComp` (b_two_one `matSub` b_one_one) in
                    let m5 = (a_one_one `matAdd` a_one_two) `matMulComp` b_two_two in
                    let m6 = (a_two_one `matSub` a_one_one) `matMulComp` (b_one_one `matAdd` b_one_two) in
                    let m7 = (a_one_two `matSub` a_two_two) `matMulComp` (b_two_one `matAdd` b_two_two) in

                        let c1 = m1 `matAdd` m4 `matSub` m5 `matAdd` m7 in
                        let c2 = m3 `matAdd` m5 in
                        let c3 = m2 `matAdd` m4 in
                        let c4 = m1 `matSub` m2 `matAdd` m3 `matAdd` m6 in

                            zipWith (++) c1 c2 ++ zipWith (++) c3 c4

                else
                let a_one_one = map (fst. vecSplit) $ (fst. matSplit) as in
                let a_one_two = map (snd . vecSplit) $ (fst. matSplit) as in
                let a_two_one = map (fst. vecSplit) $ (snd. matSplit) as in
                let a_two_two = map (snd . vecSplit) $ (snd. matSplit) as in
                let b_one_one = map (fst. vecSplit) $ (fst. matSplit) bs in
                let b_one_two = map (fst . vecSplit) $ (snd. matSplit)  bs in
                let b_two_one = map (snd. vecSplit) $ (fst. matSplit) bs in
                let b_two_two = map (snd . vecSplit) $ (snd. matSplit) bs in

                    let m1 = (a_one_one `matAdd` a_two_two) `strassen` (b_one_one `matAdd` b_two_two) in
                    let m2 = (a_two_one `matAdd` a_two_two) `strassen` b_one_one in
                    let m3 = a_one_one `strassen` (b_one_two `matSub` b_two_two) in
                    let m4 = a_two_two `strassen` (b_two_one `matSub` b_one_one) in
                    let m5 = (a_one_one `matAdd` a_one_two) `strassen` b_two_two in
                    let m6 = (a_two_one `matSub` a_one_one) `strassen` (b_one_one `matAdd` b_one_two) in
                    let m7 = (a_one_two `matSub` a_two_two) `strassen` (b_two_one `matAdd` b_two_two) in

                        let c1 = m1 `matAdd` m4 `matSub` m5 `matAdd` m7 in
                        let c2 = m3 `matAdd` m5 in
                        let c3 = m2 `matAdd` m4 in
                        let c4 = m1 `matSub` m2 `matAdd` m3 `matAdd` m6 in

                            zipWith (++) c1 c2 ++ zipWith (++) c3 c4

matSplit :: Mat -> (Mat, Mat)
matSplit mat
    | length mat > 1 = splitAt (length mat  `div` 2) mat

vecSplit :: Vec -> (Vec, Vec)
vecSplit mat
    | length mat > 1 = splitAt (length mat `div` 2) mat

matMulC :: Mat -> Float -> Mat
matMulC m c = map (map (* c)) m

vecMulC :: Vec -> Float -> Vec
vecMulC m c = map (* c) m

matAdd :: Mat -> Mat -> Mat
matAdd = zipWith vecAdd

vecAdd :: Vec -> Vec -> Vec
vecAdd= zipWith (+)

matMulComp :: Mat -> Mat -> Mat
matMulComp = zipWith (zipWith (*))

matTanh :: Mat -> Mat
matTanh = map (map tanh)

matSub :: Mat -> Mat -> Mat
matSub = zipWith (zipWith (-))

vecDot :: Vec -> Vec -> Float
-- todo : length
vecDot a b = sum (zipWith (*) a b)

perceptron :: Vec -> Vec -> Float -> (Float -> Float) -> Float
perceptron x w b act = act $ x `vecDot` w + b

matVecMul :: Mat -> Vec -> Vec
matVecMul mat vec = foldr1 vecAdd (zipWith vecMulC mat vec)

slowMatMul :: Mat -> Mat -> Mat
slowMatMul a = map (matVecMul a)

squareof2 :: Mat -> Mat
squareof2 (r:rs)
    | width  .&. (width - 1) == 0 =
        let height = length (r:rs) in
            (if ((height .&. (height - 1)) == 0) && (height == width)
                then r:rs else (
                    case height - width of
                    0 -> squareof2 $ (r:rs) ++ [replicate height 0]
                    _ -> if height > width
                        then squareof2 $ map (++ [0]) (r:rs)
                        else squareof2 $ (r:rs) ++ [replicate height 0]))
    | otherwise = let height = length (r:rs) in
            (if (((height .&. (height - 1)) == 0) && (height == width)) || (height > width)
                then squareof2 $ map (++ [0]) (r:rs)
                else squareof2 $ (r:rs) ++ [replicate height 0])
    where width = length r

reLU :: Float -> Float
reLU = max 0

sigmoid :: Float -> Float
sigmoid a = 1 / (1 + exp (-a))

mlp :: Vec -> Mat -> Vec -> (Float -> Float) -> Vec
mlp x w b act =  ("mlping.. : " ++ show ((w `matVecMul` x) `vecAdd` b))`trace` map act ((w `matVecMul` x) `vecAdd` b)

dnn :: Vec -> [Mat] -> [Vec] -> (Float -> Float) -> [Vec]
dnn x [w] [b] act = [softmax $ mlp x w b act]
dnn x (w:ws) (b:bs) act = dnn (mlp x w b act) ws bs act ++ dnn x ws bs act

softmax :: Vec -> Vec
softmax vec = map ((sum $ map exp vec) /) vec

calcGrad :: Vec -> Vec -> Vec -> Float
calcGrad (x:xs)  (y:ys) (yh:at)= (1-yh) * (yh - y) * x * yh + calcGrad xs at ys
calcGrad _ _ _ = 0

cut :: [[a]] -> ([a], [[a]])
cut a = (map head a , map tail a)

-- updateWeight :: [Vec] -> [Vec] -> [Mat] -> Mat -> Mat
-- updateWeight (x:xs) (y:ys) yhats (w:ws) = 
--     map (calcGrad x y) (map head yhats) ++ updateWeight xs ys (map tail yhats) ws

d :: Float -> Float -> Float -> Float
d x yh y=x * yh * (1-yh) * (yh - y)



vecD :: Float -> Vec -> Vec -> Vec
vecD x= zipWith (d x)

matD :: Vec -> Vec -> Vec -> Mat
matD xs yh y = map (vecD `flip` yh `flip` y) xs

ultimateD :: Mat -> Mat -> Mat -> Mat
ultimateD xss yhs ys = foldl1 matAdd (zipWith3 matD xss yhs ys)

singlelayerUpdate :: Float -> Mat -> Mat -> Mat -> Mat -> Mat
singlelayerUpdate lr w xss yhs ys = w `matSub` ( ultimateD xss yhs ys `matMulC` lr)

-------------very Tedious
-- multilayerUpdate :: Float -> [Mat] -> [Mat] -> Mat -> [Mat] -> Mat -> [Mat]
-- multilayerUpdate lr [w] [] xss [yh] ys = [singlelayerUpdate lr w xss yh ys]
-- multilayerUpdate lr flipedw [] xss (y:h:s) ys = multilayerUpdate lr (tail flipedw) [singlelayerUpdate lr (head flipedw) y h ys] xss (y:h:s) ys
-- multilayerUpdate lr flipedw new xss yhs ys = do it yourself

kLoop :: Vec -> Vec -> Vec -> Float
kLoop (w:ws) (yh:at) (y:ys) = d w yh y + kLoop ws at ys
kLoop _ _ _ = 0

jLoop :: Mat -> Float -> Vec ->  Vec -> Vec -> Vec
jLoop (w:ws) x (yh1:at1)  yh2 y = x * yh1 * (1 - yh1) * kLoop w yh2 y : jLoop  ws  x at1 yh2 y
jLoop  _ _ _ _ _ = []

iLoop :: Mat ->  [Float] -> Vec -> Vec -> Vec -> [Vec]
iLoop w x yh1 yh2 y= map (\ x -> jLoop w x yh1 yh2 y) x

nLoop :: Mat -> Mat -> Mat -> Mat -> [Vec] -> Mat
nLoop xss yh1s w yh2s ys = foldl1 matAdd (zipWith4 (iLoop w) xss yh1s yh2s ys)

hiddenUpdate :: Float -> Mat -> Mat -> Mat -> Mat -> Mat -> Mat -> (Mat, Mat)
hiddenUpdate lr w1 w2 xss yhs1 yhs2 ys = (w1 `matSub` nLoop xss yhs1 w2 yhs2 ys `matMulC` lr , singlelayerUpdate lr w2 xss yhs2 ys)
--sosleepy

hiddenFoward :: [Vec] -> Mat -> Mat -> Vec-> Vec -> (Float -> Float) -> ([Vec], [Vec])
hiddenFoward (x:xs) w1 w2 b1 b2 act = Data.Bifunctor.bimap
  (softmax (mlp x w1 b1 id) :) (mlp x w2 b2 act :)
  (hiddenFoward xs w1 w2 b1 b2 act)
hiddenFoward _ _ _ _ _ _ = ([],[])

hidden :: Float -> [Vec] -> Mat -> Mat -> Vec -> Vec -> (Float -> Float) -> [Vec] ->  [Vec]
hidden lr inputNode w1 w2 b1 b2 act outputNode =
    let (yhat2 , yhat1) =  hiddenFoward inputNode w1 w2 b1 b2 act in
    let (updatedw1, updatedw2) = hiddenUpdate lr inputNode w1 w2 yhat1 yhat2 outputNode in
    let (newyhat2, newyhat1) = hiddenFoward yhat2 w1 w2 b1 b2 act in
    let (updatedw1', updatedw2') = hiddenUpdate lr newyhat2 updatedw1 updatedw2 newyhat1 newyhat2 outputNode in
        "called" `trace`
        snd $ hiddenFoward newyhat2 updatedw1' updatedw2' b1 b2 act

mnist :: IO ()
mnist = do
    putStrLn "input file"
    filepath <- getLine
    ---------------------
    file <- readFile "dataset/mnist_train.csv"
    --테스트할 때마다 쓰기 귀찮아서 일단 이렇게함
    let input = take 40 $ (shaper . floater . split) file
    let w1 = randMatGen 2 784 100
    let w2 = randMatGen 4294 100 10
    let b1 = take 500 $ xorshift32inf 967
    let b2 = take 10 $ xorshift32inf 295
    let lr = 0.05
    let act = sigmoid
    print $ hidden lr (map snd input) w1 w2 b1 b2 act (map fst input)



randMatGen :: Int -> Int -> Int -> [[Float]]
randMatGen seed width height =
                                      let mat =  xorshift32inf seed in
                                     [ [ mat !! (i*j) | i <- [1..width] ] | j <- [1..height] ] -- 꼬우면 pull request 보내십시오.

xorshift32 :: (Num a, Bits a) => a -> a
xorshift32 seed =
    let l13seed = seed .^. (seed .<<. 13) in
    let r17seed = l13seed .^. (l13seed .>>. 17) in
    let l5seed = r17seed .^. (r17seed .<<. 5) in
        l5seed .&. 0xFFFFFFFF

xorshift32inf :: Int -> [Float]
xorshift32inf seed= map ((/ 4294967295) . fromIntegral) $ iterate xorshift32 seed