module Coil
    ( validate
    )
where

import Codec.Midi as M
import MidiUtil

-- | The Coil can play up to 800Hz
maxCoilKey :: M.Key
maxCoilKey = 79 -- (G,6), 784Hz

-- | Check that the track can be played on the Coil.
-- Checks: a) that it doesn't exceed the Coil's four voices
--         b) that 800Hz isn't exceeded
-- N.B.: This doesn't check that the track is well formed.
validate :: M.Track M.Ticks -> CheckResult
validate = execChecker . go 0
  where
    go nv ((t, M.NoteOn _ key _):es) = do
        check t (key <= maxCoilKey) "under 800Hz"
        check t (nv < 4) "only four voices"
        go (nv + 1) es
    go nv ((_, M.NoteOff _ _ _):es) = go (nv - 1) es
    go nv (_:es) = go nv es
    go nv [] = okay

