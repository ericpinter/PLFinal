{-# LANGUAGE FlexibleInstances #-}

module Lib  
    ( someFunc
    ) where
import Prelude hiding (sequence)
import Data.WAVE
import Data.List


defaultHeader :: WAVEHeader 
defaultHeader = WAVEHeader 2 48000 8 Nothing

instance Num a => Num [[a]] where
    (+) = zipWith (zipWith (+))
    (-) = zipWith (zipWith (-))
    (*) = zipWith (zipWith (*))
    negate = map (map negate)
    abs    = map (map abs) 

sawtooth :: Float -> Float -> Float -> WAVESample
sawtooth sampleRate pitch n = round $ - 2 * 200000000 / pi * atan ( 1 / tan (n / 2 * pi * pitch/sampleRate))

triangle :: Float -> Float -> Float -> WAVESample
triangle sampleRate pitch n = round $ - 2 * 200000000 / pi * asin ( sin (n / 2 * pi * pitch/sampleRate))

square :: Float -> Float -> Float -> WAVESample
square sampleRate pitch n = (*200000000) $ round $ sin(n*2*pi * pitch/sampleRate)

sine :: Float -> Float -> Float -> WAVESample
sine sampleRate pitch n = round $ 200000000*sin(n*2*pi * pitch/sampleRate)


samples :: (Float -> Float -> Float -> WAVESample) -> Float -> Float -> Float -> [[WAVESample]]
samples f sampleRate pitch numSamples = [ [f sampleRate pitch n ] | n <- [0..numSamples]]

sequence :: (Float -> Float -> Float -> WAVESample) -> Float -> [Float] -> Float -> [[WAVESample]]
sequence f sampleRate pitches numSamples = foldl (\w p -> w ++ (samples f sampleRate p numSamples)) [[]] pitches


w :: WAVE
w = WAVE defaultHeader ((sequence sine 48000 [659.26, 587.33, 523.25, 587.33, 659.26, 659.26, 659.26, 587.33, 587.33, 587.33, 659.26, 783.99, 783.99] 12000))

someFunc :: IO ()
someFunc = putWAVEFile "wave.wav" w
