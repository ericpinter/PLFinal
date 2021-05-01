{-# LANGUAGE FlexibleInstances #-}

module Lib  
    ( someFunc
    ) where

import Data.WAVE
import Data.List


defaultHeader :: WAVEHeader 
defaultHeader = WAVEHeader 2 48000 16 Nothing

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


w :: WAVE
w = WAVE defaultHeader ((samples triangle 48000 440 100000))

someFunc :: IO ()
someFunc = putWAVEFile "wave.wav" w
