module MidiUtil where

import Codec.Midi as M
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.List (foldl')



type Checker = Writer [(M.Ticks, String)] ()
type CheckResult = Either [(M.Ticks, String)] ()

execChecker :: Checker -> CheckResult
execChecker c = case execWriter c of
    [] -> Right ()
    msgs -> Left msgs

check :: M.Ticks -> Bool -> String -> Checker
check _ True _ = return ()
check t False msg = tell [(t,msg)]

okay :: Checker
okay = return ()


-- | Extract all the information for a single channel into a single track.
-- Non-channelized messages are retained, but only from tracks that have
-- at least one message for the channel.
channelTrack :: M.Channel -> M.Midi -> M.Track Ticks
channelTrack ch = foldl' M.merge [] . map onlyCh . filter hasCh . tracks
  where
    hasCh = not . null . filter ((== Just ch) . msgCh . snd)
    onlyCh = filter (maybe True (== ch) . msgCh . snd)
    msgCh m = if M.isChannelMessage m then Just (M.channel m) else Nothing

