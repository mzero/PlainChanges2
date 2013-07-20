module Preamble (preamble) where

import Euterpea

import Elements


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Preamble
-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

preamble :: Music Pitch
preamble = bassRingUp :=: onCoilRingUp coilRingUp
  where
    ringUp d p = line [ note (n*d) p | n <- [1..8]]
    bassRingUp = chord $ zipWith3 bassRingOne
        onBassStrings
        [0..]
        [(E,3), (A,3), (D,4), (G,4)]
    bassRingOne onString n p =
        onString $ bassMF $ delayM (fromIntegral n*(24*en-sn))
            $ ringUp en p :+: timesM ((3-n)*3+3) (note wn p)
    coilRingUp = chord
        [ ringTail  50 sn 0 (B,4)
        , ringTail  72 sn 0 (Fs,5)
        , ringTail  88 sn 2 (C,4)
        , ringTail 100 sn 8 (G,5)
        , ringTail 108 sn 6 (D,6)
        , ringTail 116 sn 4 (A,5)
        , ringTail 124 sn 2 (E,6)
        ] :+: final
    ringTail r d t p =
        delayM (r*en) $ ringUp d p :+: timesM t (note (8*d) p)
    final = chord $ map (note (2*hn)) [(G,5), (D,6), (A,5), (E,6)]
