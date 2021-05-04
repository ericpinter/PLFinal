module Main where

import Lib
import Prelude hiding (sequence)

main :: IO ()
main = do 
    --(saveWave "major.wav" (sequence triangle major) $ SoundSettings 2 48000 12 2 1)
    --(saveWave "majorMicro.wav" (sequence triangle major) $ SoundSettings 2 48000 19 3 2)
    --(saveWave "chromatic.wav" (sequence triangle chromatic) $ SoundSettings 2 48000 19 3 2)
    (saveWave "blah.wav" blah $ SoundSettings 3 48000 12 2 1)
    (saveWave "blahMono.wav" blahMono $ SoundSettings 1 48000 12 2 1)

major :: Sound Tune
major ss@(SoundSettings _ _ _ whole half) = scanl (\ (n,o) step -> (n + step, o)) (0,4) [whole, whole, half, whole, whole, whole, half]  `zip` (repeat $ 1/2)

chromatic :: Sound Tune
chromatic (SoundSettings _ _ numSemis _ _) = take (round numSemis) $ (iterate (\(a,b) -> (a+1,b)) (0,4)) `zip` (repeat $ 1/4)

blah :: Sound Wave
blah = do 
    l <- sequence sine maryHadALittleLamb
    r <- sequence square (transposeTune (-5) maryHadALittleLamb) 
    m <- sequence sawtooth (transposeTune (3) maryHadALittleLamb) 
    return $ channels [l,r,m]

blahMono :: Sound Wave
blahMono =  do 
    l <- sequence sine maryHadALittleLamb
    r <- sequence square (transposeTune (-5) maryHadALittleLamb) 
    m <- sequence sawtooth (transposeTune (3) maryHadALittleLamb) 
    return $ combine [l,r,m]


rest = (0,-100)

maryHadALittleLamb _ = [(5,4), (3,4), (1,4), (3,4), (5,4), (5,4), (5,4), rest, (3,4), (3,4), (3,4), rest, (5,4), (8,4), (8,4)] `zip` (repeat $ 1/4)
