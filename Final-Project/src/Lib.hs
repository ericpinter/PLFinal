{-# LANGUAGE FlexibleInstances #-}

module Lib  
    ( saveWave
    , SoundSettings (SoundSettings)
    , Sound
    , Wave
    , sine
    , square
    , triangle
    , sawtooth
    , sequence
    , channels
    , transposeTune
    ) where
import Prelude hiding (sequence)
import Data.WAVE
import Data.List
import Data.Int (Int32)

instance Num a => Num [[a]] where
    (+) = zipWithLonger (zipWithLonger (+))
    (-) = zipWithLonger (zipWithLonger (-))
    (*) = zipWithLonger (zipWithLonger (*))
    negate = map (map negate)
    abs    = map (map abs) 

zipWithLonger :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithLonger f = zipLonger f id id 


channels :: [Wave] -> Wave
channels = foldl (zipWithLonger (++)) []    


zipLonger :: (a -> b -> c) -> (a -> c) -> (b-> c) -> [a] -> [b] -> [c]
zipLonger _ _ _ [] [] = []
zipLonger _ _ h [] bs = map h bs
zipLonger _ g _ as [] = map g as 
zipLonger f g h (a:as) (b:bs) = f a b : zipLonger f g h as bs

                        --contains sample rate, number of semitones in the tuning    
data SoundSettings = SoundSettings Float Float --TODO add number of channels, things like that

type Sound = (->) SoundSettings


type WaveFunction = Float -> Float -> Float -> Sound WAVESample
type Wave = [[WAVESample]]
type Pitch = (Int,Int)
type Duration = Rational
type Note = (Pitch,Duration)
type Tune = [Note]

frequencyOf :: Pitch -> Sound Float
frequencyOf (n, octave) (SoundSettings _ semitones) =  16.35 * (2 ** ((fromIntegral n)/semitones + fromIntegral octave)) -- 16.35 is the frequency of C0. 


maxAmplitude = fromIntegral (maxBound :: Int32)


sawtooth :: WaveFunction
sawtooth freq amp n (SoundSettings sampleRate _) = round $ - 2 * (maxAmplitude * (2**amp) / pi) * atan ( 1 / tan (n / 2 * pi * freq/sampleRate))

triangle :: WaveFunction
triangle freq amp n (SoundSettings sampleRate _) = round $ - 2 * (maxAmplitude * (2**amp) / pi) * asin ( sin (n / 2 * pi * freq/sampleRate))

square :: WaveFunction
square freq amp n (SoundSettings sampleRate _) = round $ (\a -> a * maxAmplitude * (2**amp)) $ signum $ sin(n*2*pi * freq/sampleRate)

sine :: WaveFunction
sine freq amp n (SoundSettings sampleRate _) = round $ (maxAmplitude * (2**amp)) * sin(n*2*pi * freq/sampleRate)


samples' :: Float -> WaveFunction -> Float -> Duration -> Sound Wave
samples' amp f freq beats ss@(SoundSettings sampleRate _) = [ [f freq amp n ss] | n <- [0.. sampleRate * fromRational beats]]

samples :: WaveFunction -> Float -> Duration -> Sound Wave
samples = samples' (-2)

sequence :: WaveFunction -> Tune -> Sound Wave
sequence f notes = do 
    notes' <- mapM (\(pitch,duration) -> do --lookup pitches based on the sound settings
        freq <- frequencyOf pitch
        return (freq, duration)) notes 
    \ss -> foldl (\w (freq,duration) -> w ++ (samples f freq duration ss)) [[]] notes' 


transposeTune :: Int -> Tune -> Tune
transposeTune semitones = map (\((a,b),d) -> ((a+semitones,b),d))


saveWave :: Wave -> Sound (IO ())
saveWave w (SoundSettings sampleRate _) = putWAVEFile "wave.wav" $ WAVE (WAVEHeader 2 (round sampleRate) 8 Nothing) $ w
