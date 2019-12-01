# Nine-Mens-Morris

This is a rough, simple Haskell implementation of the game of Nine Men's Morris according to the P16 Challenge competition rules document (not publicly available). 

There are obvious improvements that ought to be made - such as using the State monad transformer to combine with the IO monad. This would allow much simpler extension of the game to include the 'extended challenge' features such as 'undo'. Other improvements could be proper handling of invalid-state exceptions (though invalid moves are handled properly).

## How to use

Compile it using GHC or load Main.hs in GHCi and run `main`.

## What is the Bristol Post-16 Programming Challenge?

It is an annual competition, organised by Terry Watts (Cotham School) and Caroline Higgins (University of Bristol) involving students over the age of 16 from a number of schools (5 or more) from across Bristol (UK). Prior to 2019 is was hosted at the university. In 2019 it was hosted at IBM Bristol. 

The event involves students competing to develop a small game within 4.5 hours in the afternoon. Students may use any language to complete the challenge. This year, the game was 9 Men's Morris. Clear guidelines are laid out with stages/phases of completion for the students to achieve. A team of computer scientists from IBM and UoB students spend time helping the students during the competition and the judge the code. Key judging areas include on-paper planning, program functionality (including completeness, edge cases, performance, creativity, etc), programming style, program structure, simplicity, testing, etc. 

Before the game making begins, short talks from IBM staff and UoB Computer Science students are given on the topic of careers in and routes into Computer Science.
