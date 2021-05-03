module Main where

import Lib
import Prelude hiding (sequence)

main :: IO ()
main = do 
    (do song <- blah
        saveWave song) $ SoundSettings 48000 12


blah :: Sound Wave
blah = do 
    l <- sequence sine maryHadALittleLamb
    r <- sequence square (transposeTune 5 maryHadALittleLamb) 
    return $ channels [l,r]

rest = (0,-100)

maryHadALittleLamb = [(5,4), (3,4), (1,4), (3,4), (5,4), (5,4), (5,4), rest, (3,4), (3,4), (3,4), rest, (5,4), (8,4), (8,4)] `zip` (repeat $ 1/4)
--maryHadALittleLamb =  [659.26, 587.33, 523.25, 587.33, 659.26, 659.26, 659.26, 587.33, 587.33, 587.33, 659.26, 783.99, 783.99]
