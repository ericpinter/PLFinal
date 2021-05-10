# Final-Project

## Project Summary
Our project is a music generating language that allows the user to provide various forms of input to produce interesting and unique tunes/sound waves. There are various sound waves (sine, triangle, square, and sawtooth), which can be played at any frequency and any amplitude. The language can handle adding multiple sound waves together in a sequence or playing the waves concurrently. The user can provide a series of basic frequencies or (ideally) provide descriptions of notes and how long they should be played (in fractions of a second). Rests can be added to these sequences as well. The user can provide some low level properties they desire in the file such as sample rate and number of channels. They can also experiment with alternate (equal temperment) tunings with a particular number of semitones. They can even define music in terms of properties of the tuning that they are working in. For example, app/Main.hs includes an implementation of the major scale which uses the exact same code for both normal 12 tone tuning and a very odd 73 tone tuning, and gives similar (but distinct) results.

## Examples
The file app/Main.hs contains examples of how to define basic tunes (like mary had a little lamb), more generalized tunes like the major and chromatic scales, and how to use the sound monad to render and combine waves. It also shows how to actually save these waves via the saveWave function.

## Project Setup
First, look at src/Lib.hs where you will find the implementation of our language and its features which extend/use the Data.WAVE package. Then you can look at app/Main.hs where you will see the use of our language and examples of how it can be used. The easiest way to experiment with our language is to simply write tunes in app/Main.hs and modify the main method to save whatever waves you create. You can then simply use `stack run`.

## Effort
We spent a significant amount of time learning about (basic) music theory, since neither of us knew much about it to begin with. We also spent a decent amount of time talking about Haskell features like do notation and various standard library functions, since Ashlynn was not familiar with how they worked and they were vital in making the implementation of our language as compact as it is.
