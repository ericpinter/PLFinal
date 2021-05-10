# Final-Project

## Project Summary
Our project is a music generating language that allows the user to provide various forms of input to produce interesting and unique tunes/sound waves. There are various sound waves (sine, triangle, square, and sawtooth). The underlying code incorporates pitch and amplitude and accounts for making the sounds loud enough. The language can handle adding multiple sound waves together in a sequence or also combining the waves together. The user can provide a series of basic frequencies or provide pairs of notes and octaves to represent a tune. Rests can be added to these sequences as well. The user has the opportunity to set the sample rate, number of channels, semitones, and the size of the half and whole steps when provided a sequence of notes to make a sound wave. The user just has to provide which type of wave, the duration of the sound, frequency, amplitude, and the sequence of notes to produce the sound wave. Scales can also be easily formulated when the starting note is provided such as the major scale which is included that is based on a series of half and whole steps.

## Project Setup
First, look at Lib.hs where you will find the implementation of our language and its features which extend/use the Data.WAVE package. Then you can look at Main.hs where you will see the use of our language and examples of how it can be used. 

## Effort
