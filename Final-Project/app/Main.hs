{-# OPTIONS_GHC -Wall #-}
module Main where

import Lib
import Prelude hiding (sequence)

main :: IO ()
main = do 
    saveWave "major.wav" (sequence triangle major) $ SoundSettings 1 48000 12 2 1
    saveWave "majorMicro.wav" (sequence triangle major) $ SoundSettings 1 48000 73 13 4
    saveWave "chromatic.wav" (sequence triangle chromatic) $ SoundSettings 1 48000 19 3 2
    saveWave "channels.wav" showOffChannels $ SoundSettings 3 48000 12 2 1
    saveWave "combine.wav" showOffCombine $ SoundSettings 1 48000 12 2 1
    saveWave "waitWhat.wav" (sequence triangle maryHadALittleLamb) $ SoundSettings 1 48000 (4 * pi) 3 2

major :: Sound Tune
major (SoundSettings _ _ _ whole half) = scanl (\ (n,o) step -> (n + step, o)) (0,4) [whole, whole, half, whole, whole, whole, half]  `zip` (repeat $ 1/4)

chromatic :: Sound Tune
chromatic (SoundSettings _ _ numSemis _ _) = take (round numSemis) $ (iterate (\(a,b) -> (a+1,b)) (0,4)) `zip` (repeat $ 1/8)

showOffChannels :: Sound Wave
showOffChannels = do 
    l <- sequence sine maryHadALittleLamb
    r <- sequence square (transposeTune (-5) maryHadALittleLamb) 
    m <- sequence sawtooth (transposeTune (7) maryHadALittleLamb) 
    return $ channels [l,r,m]

showOffCombine :: Sound Wave
showOffCombine =  do 
    l <- sequence sine maryHadALittleLamb
    r <- sequence square (transposeTune (-5) maryHadALittleLamb) 
    m <- sequence sawtooth (transposeTune (7) maryHadALittleLamb) 
    return $ combine [l,r,m]

maryHadALittleLamb :: Sound Tune
maryHadALittleLamb _ = [(5,4), (3,4), (1,4), (3,4), (5,4), (5,4), (5,4), rest, (3,4), (3,4), (3,4), rest, (5,4), (8,4), (8,4)] `zip` (repeat $ 1/4)
