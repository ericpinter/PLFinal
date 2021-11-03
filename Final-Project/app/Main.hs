{-# OPTIONS_GHC -Wall #-}
module Main where

import Lib
--import qualified Prelude as P (sequence)
import Prelude hiding (sequence)


main :: IO ()
main = do 
    --saveWave "major.wav" (sequence triangle major) $ SoundSettings 1 48000 16 12 2 1
    --saveWave "majorMicro.wav" (sequence triangle major) $ SoundSettings 1 48000 16 73 13 4
    --saveWave "chromatic.wav" (sequence triangle chromatic) $ SoundSettings 1 48000 16 19 3 2
    --saveWave "channels.wav" showOffChannels $ SoundSettings 3 48000 16 12 2 1
    --saveWave "combine.wav" showOffCombine $ SoundSettings 1 48000 16 12 2 1
    saveWave "waitWhat.wav" (sequence triangle maryHadALittleLamb) $ SoundSettings 1 48000 16 (4 * pi) 3 2
    --saveWave "sqTween.wav" (sequence square $ const [(squareFreqTween (0,1) (1,7), 1)]) $ SoundSettings 1 48000 16 24 3 2
    --saveWave "awful.wav" (sequence square $ const [(squareFreqTween (0,1) (1,7), 1)]) $ SoundSettings 1 48000 1 24 3 2
    --saveWave "linearTween.wav" (sequence square $ const [(linearFreqTween (0,1) (1,4), 1/2)]) $ SoundSettings 1 48000 16 24 3 2
    saveWave "test.wav" (sequence sine $ const $ simply [((0,4),1/100),((1,4),1/100)]) $ SoundSettings 1 48000 16 24 4 2



major :: Sound Tune
major (SoundSettings _ _ _ _ wholeInt halfInt) = simply $ scanl (\ (n,o) step -> (n + step, o)) (0,4) [whole, whole, half, whole, whole, whole, half]  `zip` repeat (1/4)
   where whole = fromIntegral wholeInt
         half = fromIntegral halfInt

chromatic :: Sound Tune
chromatic (SoundSettings _ _ _ numSemis _ _) = simply $ take (round numSemis) $ iterate (\(a,b) -> (a+1,b)) (0,4) `zip` repeat (1/8)

showOffChannels :: Sound Wave
showOffChannels = do 
    l <- sequence sine maryHadALittleLamb
    r <- sequence square (transposeTune (-5) maryHadALittleLamb) 
    m <- sequence sawtooth (transposeTune 7 maryHadALittleLamb)
    return $ channels [l,r,m]

showOffCombine :: Sound Wave
showOffCombine =  do 
    l <- sequence sine maryHadALittleLamb
    r <- sequence square (transposeTune (-5) maryHadALittleLamb) 
    m <- sequence sawtooth (transposeTune 7 maryHadALittleLamb)
    return $ combine [l,r,m]

maryHadALittleLamb :: Sound Tune
maryHadALittleLamb _ = simply $ [(5,4), (3,4), (1,4), (3,4), (5,4), (5,4), (5,4), rest, (3,4), (3,4), (3,4), rest, (5,4), (8,4), (8,4)] `zip` repeat (1/4)

tween :: Sound Tune
tween = const [(squareFreqTween (0,1) (1,4), 1/2)]

rattlesnake :: Sound Tune
rattlesnake (SoundSettings _ _ _ _ wholeInt halfInt) =simply $ scanl (\ (n,o) step -> (n + step, o)) (0,4) [half+1, 1+half, whole, half,-half,-whole,-(1+half)]  `zip` repeat (1/2)
   where whole = fromIntegral wholeInt
         half = fromIntegral halfInt

rattlesnakeFull :: Sound Wave
rattlesnakeFull = do
  l <- sequence sine $ transposeTune 12 rattlesnake
  r <- sequence sine $ transposeTune (-12) rattlesnake
  return $ combine [l,r]
