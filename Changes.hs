module Changes where

import Data.List (inits, tails)


{-
  See Tintinnalogia, or, the Art of Ringing
      by Richard Duckworth and Fabian Stedman (1671)
      http://www.gutenberg.org/ebooks/18567

  Also Change-ringing Disentangled: With Hints on the Direction of Belfries,
       on the Management of Bells, Etc. Etc. Etc
       by Woolmore Wigram (1882)
       http://openlibrary.org/books/OL24178800M/Change-ringing_disentangled
-}

{-
    This is based on the original code written in PLA. That code, in the file
    PLAIN[1,MGL], is written as generators of permutations of 1..n. However,
    when those permutations are played, (see AHHA[1,MGL]) they are played from
    the end to the front of each permutation. Further, the numbers index an
    ascending array of pitches. The result is that, for example, the permutation
    {1,2,3} played on the pitches {FS,GS,AS} results in a *descending* line.

    This code 'uncoils' the above details so that the changes generated are in
    played order, first to last. Further, this code generalizes by taking a
    sequence of pitches that corresponds to the first change, and hence is
    traditionally (but here needn't be) descending ptiches.

    The PLA code implemented Plain Changes, but chose a different hunt pattern
    for 3 bells vs. 4 and 5. This code extends the choice for 4 and 5 bells to
    higher numbers.

    These are forms of Plain Changes implemented:

    For |1| and |2| they are the only form.
    For |3| Tenor hunt down
    For |4| Treble hunt up, extremes between the 2 nearest to it
    For |5| Treble is the whole hunt, the second the half hunt, both hunting
            up, and the extreme changes are those nearest the half hunt

    Remember that 'hunt down' means toward the start of the change; 'treble'
    and 'tenor' in this code refer to first and last pitch of the supplied
    first change (which, if descending pitches, is indeed trebel and tenor).
-}

ringOf :: [a] -> [[a]]
ringOf [] = [[]]
ringOf [a] = [[a]]

ringOf [a, b] =
    [ [a, b]
    , [b, a]
    ]

ringOf [a, b, c] =
    [ [a, b, c]
    , [a, c, b]
    , [c, a, b]
    , [c, b, a]
    , [b, c, a]
    , [b, a, c]
    ]

ringOf (tenor:rest) = upDown $ ringOf rest
  where
    upDown (c1:c2:cs) = up c1 ++ down c2 ++ upDown cs
    upDown [c0] = up c0
    upDown [] = []

    up bells = zipWith (\h t -> h ++ tenor : t) (inits bells) (tails bells)
    down = reverse . up

