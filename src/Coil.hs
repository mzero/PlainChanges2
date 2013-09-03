module Coil
    ( validate
    , restrict
    , dropAnOctave
    , Messages
    )
where

import Codec.Midi as M
import Data.Either (partitionEithers)
import Data.Maybe (mapMaybe)
import MidiUtil

-- | The Coil can play up to 800Hz
maxCoilKey :: M.Key
maxCoilKey = 79 -- (G,6), 784Hz

data CoilError = MaxPolyphonyExceeded Int | Over800Hz Key
  deriving (Eq, Show)

-- | Check that the track can be played on the Coil.
-- Checks: a) that it doesn't exceed the Coil's four voices
--         b) that 800Hz isn't exceeded
-- N.B.: This doesn't check that the track is well formed.
validate :: TrackChecker CoilError
validate = go 0
  where
    go nv ((t, M.NoteOn _ key _):es) = do
        check t (key <= maxCoilKey) $ Over800Hz key
        check t (nv < 4) $ MaxPolyphonyExceeded (nv + 1)
        go (nv + 1) es
    go nv ((_, M.NoteOff _ _ _):es) = go (nv - 1) es
    go nv (_:es) = go nv es
    go _ [] = okay

type Messages = [(M.Time, CoilError)]

restrict :: M.Track M.Time -> (Messages, M.Track M.Time)
restrict = partitionEithers . mapMaybe check
  where
    check e@(t, M.NoteOn _ key _) | key <= maxCoilKey = Just $ Right e
                                  | otherwise = Just $ Left (t, Over800Hz key)
    check e@(_, M.NoteOff _ key _) | key <= maxCoilKey = Just $ Right e
                                   | otherwise = Nothing
    check e = Just $ Right e

dropAnOctave :: Channel -> M.Track a -> M.Track a
dropAnOctave ch = map adjust
  where
    adjust (t, NoteOn  c k v) | c == ch && k >= 12 = (t, NoteOn  c (k - 12) v)
    adjust (t, NoteOff c k v) | c == ch && k >= 12 = (t, NoteOff c (k - 12) v)
    adjust e = e
