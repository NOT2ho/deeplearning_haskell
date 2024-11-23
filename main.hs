module Main where
import Data.Bits

type Mat = [Vec]
type Vec = [Float]

main  :: IO()
main = do
    error "userfault"

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
mlp x w b act = map act ((w `matVecMul` x) `vecAdd` b)

dnn :: Vec -> [Mat] -> [Vec] -> (Float -> Float) -> Vec
dnn x (w:ws) (b:bs) act = dnn (mlp x w b act) ws bs act
dnn x [] [] _ = x


