module Lib  
    ( someFunc
    ) where

import Data.WAVE
import Data.List

defaultHeader :: WAVEHeader 
defaultHeader = WAVEHeader 2 48000 16 Nothing

sinSamples :: Float -> Float -> [[WAVESample]]
sinSamples sampleRate pitch = [ [round $ 200000000*sin(n*2*pi * pitch/sampleRate), something ]| n <- [1..100000], ]   

w :: WAVE
w = WAVE defaultHeader (transpose ( transpose (sinSamples 48000 440) ++ transpose(sinSamples 48000 470))) 

someFunc :: IO ()
someFunc = putWAVEFile "wave.wav" w
