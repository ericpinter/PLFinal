{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}


module Lib where
import Prelude hiding (sequence)
import Data.WAVE
import Data.Int (Int32)

-- This is similar to the prelude function zipWith, except that zipWith will stop combining the lists when one runs out of elements
-- However if we want to express two sounds happening at the same time we don't want the longer one to be cut short
zipWithLonger :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithLonger f = zipLonger f id id 

-- The channels function combines waves into different channels in a single wave.
-- This can be used to assign what plays through the left and right speakers. 

-- Pay attention to how many channels you use, as this should be the first number you give SoundSettings.
-- Be careful about using channels mutiple times, since it can lead to the channels being split up in unexpected ways.
channels :: [Wave] -> Wave
channels = foldl (zipWithLonger (++)) []    

-- Similar to channels, but overlays multiple waves ontop of each other in the same channel.
combine :: [Wave] -> Wave
combine = foldl (zipWithLonger (zipWithLonger (+))) []

zipLonger :: (a -> b -> c) -> (a -> c) -> (b-> c) -> [a] -> [b] -> [c]
zipLonger _ _ _ [] [] = []
zipLonger _ _ h [] bs = map h bs
zipLonger _ g _ as [] = map g as 
zipLonger f g h (a:as) (b:bs) = f a b : zipLonger f g h as bs

                        --contains # of channels, sample rate, number of semitones in the tuning, whole step, half step  
data SoundSettings = SoundSettings Int Float Float Int Int

-- The type (Sound a) is just another way of talking about a function that returns an a for particular sound settings.
-- Note that we can just ignore the settings, so any type a can be turned into a (Sound a) easily. 
type Sound = (->) SoundSettings

-- A Wave Function takes the frequency, amplitude, and phase and returns the sample of that wave at that particular phase.
type WaveFunction = Float -> Float -> Float -> Sound WAVESample
type Wave = [[WAVESample]]
type Pitch = (Int,Int) -- First number is which semitone in the octave the note is, second number is which octave to use.

-- Kind of a hack, but for our purposes a rest is just something with a really really low frequency.
rest :: Pitch
rest = (0,-100)

type Duration = Rational
type Note = (Pitch,Duration)
type Tune = [Note]

-- The pitch (0,0) corresponds to C0 and is defined as the frequency 16.35
-- Adding one to the second value transposes up an octave, via doubling the frequency
-- Adding one to the first value transposes up a semitone. For example in a normal tuning with 12 semitones to an octave it entails multiplying the frequency by the twelfth root of two
frequencyOf :: Pitch -> Sound Float
frequencyOf (n, octave) (SoundSettings _ _ semitones _ _) =  16.35 * (2 ** ((fromIntegral n)/semitones + fromIntegral octave))

maxAmplitude :: Float
maxAmplitude = fromIntegral (maxBound :: Int32)

-- There are four types of waves that are pretty simple to make using trigonometry. Thanks for the formulae Wikipedia!
sawtooth :: WaveFunction
sawtooth freq amp n (SoundSettings _ sampleRate _ _ _) = round $ - 2 * (maxAmplitude * (2**amp) / pi) * atan ( 1 / tan (n * pi * freq/sampleRate))

triangle :: WaveFunction
triangle freq amp n (SoundSettings _ sampleRate _ _ _) = round $ - 2 * (maxAmplitude * (2**amp) / pi) * asin ( sin (n * 2 * pi * freq/sampleRate))

square :: WaveFunction
square freq amp n (SoundSettings _ sampleRate _ _ _) = round $ (\a -> a * maxAmplitude * (2**amp)) $ signum $ sin(n * 2 * pi * freq/sampleRate)

sine :: WaveFunction
sine freq amp n (SoundSettings _ sampleRate _ _ _) = round $ (maxAmplitude * (2**amp)) * sin(n * 2 * pi * freq/sampleRate)

-- samples' takes an amplitude, type of wave, a frequency, and a duration and generates a wave with those properties
samples' :: Float -> WaveFunction -> Float -> Duration -> Sound Wave
samples' amp f freq beats ss@(SoundSettings _ sampleRate _ _ _) = [ [f freq amp n ss] | n <- [0.. sampleRate * fromRational beats]]

-- samples just mirrors samples' with a default wave amplitude
samples :: WaveFunction -> Float -> Duration -> Sound Wave
samples = samples' (-2)

-- Takes a type of wave and a Tune (i.e. a bunch of notes) and generates the wave that plays them sequentially.
sequence :: WaveFunction -> Sound Tune -> Sound Wave
sequence f st = do 
    tune <- st
    waves <- mapM (\(pitch,duration) -> do 
        freq <- frequencyOf pitch     -- lookup frequencies based on the sound settings
        samples f freq duration) tune -- get the wave associated with said frequency
    return $ foldl (++) [] waves

-- shifts each note in the tune up by a certain number of semitones
transposeTune :: Int -> Sound Tune -> Sound Tune
transposeTune semitones tune = map (\((a,b),d) -> ((a+semitones,b),d)) . tune

saveWave :: String -> Sound Wave -> Sound (IO ())
saveWave fileName w ss@(SoundSettings numChannels sampleRate _ _ _) = putWAVEFile fileName $ WAVE (WAVEHeader numChannels (round sampleRate) 8 Nothing) $ w ss
