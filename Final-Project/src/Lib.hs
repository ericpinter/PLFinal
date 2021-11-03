{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Lib where

import Data.Int (Int32)
import Data.WAVE
import Prelude hiding (sequence)
import Data.Bifunctor
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

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

zipLonger :: (a -> b -> c) -> (a -> c) -> (b -> c) -> [a] -> [b] -> [c]
zipLonger _ _ _ [] [] = []
zipLonger _ _ h [] bs = map h bs
zipLonger _ g _ as [] = map g as
zipLonger f g h (a : as) (b : bs) = f a b : zipLonger f g h as bs

--contains # of channels, sample rate, quality (bit resolution of samples), number of semitones in the tuning, whole step, half step
data SoundSettings = SoundSettings Int Float Int Float Int Int

-- The type (Sound a) is just another way of talking about a function that returns an a for particular sound settings.
-- Note that we can just ignore the settings, so any type a can be turned into a (Sound a) easily.
type Sound = (->) SoundSettings

--waves defined between 0.0 and 1.0
type Waveoid a = Float -> a

type WaveFunction = Waveoid WAVESample

type Wave = [[WAVESample]]

type Pitch = (Float, Float) -- First number is which semitone in the octave the note is, second number is which octave to use.

-- Kind of a hack, but for our purposes a rest is just something with a really really low frequency.
rest :: Pitch
rest = (0, -100)

type Duration = Rational

type Note = (Waveoid Pitch, Duration)

simpleNotes :: (Pitch, Duration) -> Note
simpleNotes (a, b) = (const a, b)

simply = map simpleNotes

type Tune = [Note]

-- The pitch (0,0) corresponds to C0 and is defined as the frequency 16.35
-- Adding one to the second value transposes up an octave, via doubling the frequency
-- Adding one to the first value transposes up a semitone. For example in a normal tuning with 12 semitones to an octave it entails multiplying the frequency by the twelfth root of two
frequencyOf :: Pitch -> Sound Float
frequencyOf (n, octave) (SoundSettings _ _ _ semitones _ _) = 16.35 * (2 ** (n / semitones + octave))

parallelTween :: (Float -> Float -> Float -> Float) -> Pitch -> Pitch -> Waveoid Pitch
parallelTween f (a1,a2) (b1,b2) percent = (f a1 b1 percent, f a2 b2 percent)

linearFreqTween :: Pitch -> Pitch -> Waveoid Pitch
linearFreqTween = parallelTween (\a b percent -> b * percent + a * (1 - percent))


squareFreqTween :: Pitch -> Pitch -> Waveoid Pitch
squareFreqTween = parallelTween (\a b percent -> b * percent ** 2 + a * (1 - (percent ** 2)))


maxAmplitude :: Float
maxAmplitude = fromIntegral (maxBound :: Int32)

normalize :: Float -> Float -> Int32
normalize amp a = round $ maxAmplitude * (2 ** amp) * a

-- A Basic wave takes the frequency, amplitude, and phase and returns the sample of that wave at that particular phase.
type BaseWave = Float -> Float -> Sound WaveFunction

-- There are four types of waves that are pretty simple to make using trigonometry. Thanks for the formulae Wikipedia!
sawtooth :: BaseWave
sawtooth freq amp (SoundSettings _ sampleRate _ _ _ _) n = normalize amp $ (-2) / pi * atan (1 / tan (n * pi * freq / sampleRate))

triangle :: BaseWave
triangle freq amp (SoundSettings _ sampleRate  _ _ _ _) n = normalize amp $ (-2) / pi * asin (sin (n * 2 * pi * freq / sampleRate))

square :: BaseWave
square freq amp (SoundSettings _ sampleRate _ _ _ _) n = normalize amp $ signum (sin (n * 2 * pi * freq / sampleRate))

sine :: BaseWave
sine freq amp (SoundSettings _ sampleRate _ _ _ _) n = normalize amp $ sin (n * 2 * pi * freq / sampleRate)

--finds the point in a wave where the value is closest to the given sample value
findSample :: WAVESample -> Waveoid Float -> Waveoid (Sound Float) -> BaseWave -> Sound Float
findSample value amp freq wave ss@(SoundSettings _ sampleRate _ _ _ _) = fromIntegral (fromJust (elemIndex (minimum list) list))
 where list = [abs (fromIntegral (wave (freq (percent n) ss) (amp $ percent n) ss n) - value) | n <- [0 .. sampleRate ]]
       percent n = n / sampleRate


-- samples' takes an amplitude, type of wave, a frequency, and a duration and generates a wave with those properties
samples' :: Waveoid Float -> BaseWave -> Waveoid (Sound Float) -> Duration -> WAVESample -> Sound Wave
samples' amp f freq beats value ss@(SoundSettings _ sampleRate _ _ _ _) = [[f (freq (percent n) ss) (amp $ percent n) ss n] | n <- [sampleOffset  .. sampleOffset + numSamples]]
  where
    percent :: Float -> Float
    percent n = n / (sampleOffset + numSamples)

    sampleOffset :: Float
    sampleOffset = findSample value amp freq f ss

    numSamples :: Float
    numSamples = sampleRate * fromRational beats


-- samples just mirrors samples' with a default wave amplitude
samples :: BaseWave -> (Float -> Sound Float) -> Duration -> WAVESample -> Sound Wave
samples = samples' $ const (-2)

-- Takes a type of wave and a Tune (i.e. a bunch of notes) and generates the wave that plays them sequentially.
sequence :: BaseWave -> Sound Tune -> Sound Wave
sequence f st ss = concat waves
  where tune = st ss
        waves = tail $ scanl func [[0 :: WAVESample]] tune -- get the wave associated with said frequency
        func lastwave (a, b)  = samples f (frequencyOf . a) b (fromIntegral $ head (last lastwave)) ss
        --traceFunc (a,b) lastwave = trace ("Prev val " ++ show (head (last lastwave)) ++ " of rank " ++  show (length lastwave) ++ ", " ++ show (length $ head lastwave) ++ " first new val" ++ show (head $ head (func (a,b) lastwave))) $ func (a,b) lastwave
        --traceFunc lastwave (a,b) = trace ("Prev val " ++ show (lastwave) ++ " first new val" ++ show (func (a,b) lastwave)) $ func (a,b) lastwave

  --return $ concat waves

-- shifts each note in the tune up by a certain number of semitones
transposeTune :: Float -> Sound Tune -> Sound Tune
transposeTune semitones tune = map (Data.Bifunctor.first (shift semitones .)) . tune

shift :: Float -> Pitch -> Pitch
shift semitones = Data.Bifunctor.first (+ semitones)

saveWave :: String -> Sound Wave -> Sound (IO ())
saveWave fileName w ss@(SoundSettings numChannels sampleRate quality _ _ _) = putWAVEFile fileName $ WAVE (WAVEHeader numChannels (round sampleRate) quality Nothing) $ w ss
