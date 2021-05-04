{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}


module Lib  
    ( saveWave
    , SoundSettings (SoundSettings)
    , Sound
    , Wave
    , Tune
    , sine
    , square
    , triangle
    , sawtooth
    , sequence
    , channels
    , combine
    , transposeTune
    ) where
import Prelude hiding (sequence)
import Data.WAVE
import Data.Int (Int32)


zipWithLonger :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithLonger f = zipLonger f id id 

channels :: [Wave] -> Wave
channels = foldl (zipWithLonger (++)) []    

combine :: [Wave] -> Wave
combine = foldl (zipWithLonger (zipWithLonger (+))) []



zipLonger :: (a -> b -> c) -> (a -> c) -> (b-> c) -> [a] -> [b] -> [c]
zipLonger _ _ _ [] [] = []
zipLonger _ _ h [] bs = map h bs
zipLonger _ g _ as [] = map g as 
zipLonger f g h (a:as) (b:bs) = f a b : zipLonger f g h as bs

                        --contains num channels, sample rate, number of semitones in the tuning, whole step, half step  
data SoundSettings = SoundSettings Int Float Float Int Int

type Sound = (->) SoundSettings


type WaveFunction = Float -> Float -> Float -> Sound WAVESample
type Wave = [[WAVESample]]
type Pitch = (Int,Int)
type Duration = Rational
type Note = (Pitch,Duration)
type Tune = [Note]

frequencyOf :: Pitch -> Sound Float
frequencyOf (n, octave) (SoundSettings _ _ semitones _ _) =  16.35 * (2 ** ((fromIntegral n)/semitones + fromIntegral octave)) -- 16.35 is the frequency of C0. 

maxAmplitude :: Float
maxAmplitude = fromIntegral (maxBound :: Int32)


sawtooth :: WaveFunction
sawtooth freq amp n (SoundSettings _ sampleRate _ _ _) = round $ - 2 * (maxAmplitude * (2**amp) / pi) * atan ( 1 / tan (n / 2 * pi * freq/sampleRate))

triangle :: WaveFunction
triangle freq amp n (SoundSettings _ sampleRate _ _ _) = round $ - 2 * (maxAmplitude * (2**amp) / pi) * asin ( sin (n / 2 * pi * freq/sampleRate))

square :: WaveFunction
square freq amp n (SoundSettings _ sampleRate _ _ _) = round $ (\a -> a * maxAmplitude * (2**amp)) $ signum $ sin(n*2*pi * freq/sampleRate)

sine :: WaveFunction
sine freq amp n (SoundSettings _ sampleRate _ _ _) = round $ (maxAmplitude * (2**amp)) * sin(n*2*pi * freq/sampleRate)


samples' :: Float -> WaveFunction -> Float -> Duration -> Sound Wave
samples' amp f freq beats ss@(SoundSettings _ sampleRate _ _ _) = [ [f freq amp n ss] | n <- [0.. sampleRate * fromRational beats]]

samples :: WaveFunction -> Float -> Duration -> Sound Wave
samples = samples' (-2)

sequence :: WaveFunction -> Sound Tune -> Sound Wave
sequence f notes = do 
    notes'  <- notes
    notes'' <- mapM (\(pitch,duration) -> do --lookup pitches based on the sound settings
        freq <- frequencyOf pitch
        return (freq, duration)) notes'
    \ss -> foldl (\w (freq,duration) -> w ++ (samples f freq duration ss)) [[]] notes'' 


transposeTune :: Int -> Sound Tune -> Sound Tune
transposeTune semitones soundSettings = map (\((a,b),d) -> ((a+semitones,b),d)) . soundSettings


saveWave :: String -> Sound Wave -> Sound (IO ())
saveWave fileName w ss@(SoundSettings numChannels sampleRate _ _ _) = putWAVEFile fileName $ WAVE (WAVEHeader numChannels (round sampleRate) 8 Nothing) $ w ss
