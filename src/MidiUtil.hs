module MidiUtil
    ( Checker, CheckResult
    , check, okay

    , extractChannel

    , TrackChecker
    , checkChannel, runCheckChannel
    , processChannel
    )
where

import Codec.Midi as M
import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Data.List (foldl', partition)


type Checker err = Writer [(M.Time, err)] ()
type CheckResult err = Either [(M.Time, err)] ()

execChecker :: Checker err -> CheckResult err
execChecker c = case execWriter c of
    [] -> Right ()
    errs -> Left errs

check :: M.Time -> Bool -> err -> Checker err
check _ True _ = return ()
check t False err = tell [(t, err)]

okay :: Checker a
okay = return ()


-- | Extract eveything for a single channel from a group of tracks into a single
-- track, and a collections of tracks of the remainder. Tracks which don't
-- contain the channel at all are included verbatim in the remainder. Tracks
-- which contain only the channel, are merged fully into the single track.
-- Tracks which mix the channel with others, are divided, with events for the
-- channel merged into the other channel's tracks, and the rest included in the
-- remainder.
partitionChannel :: (Num a, Ord a) => M.Channel -> [M.Track a] -> (M.Track a, [M.Track a])
partitionChannel ch trks = (chTrk, noChTrks)
  where
    (trks', noChTrks1) = partition (trackHasChannel ch) trks
    (mixedTracks, chTrks1) = partition (trackHasOtherChannel ch) trks'
    (chTrks2, noChTrks2) = unzip $
        map (partition $ messageOnChannel ch . snd) mixedTracks
    chTrk = foldl' M.merge [] $ chTrks1 ++ chTrks2
    noChTrks = noChTrks1 ++ noChTrks2

-- | Does a track have events on a given channel.
trackHasChannel :: M.Channel -> M.Track a -> Bool
trackHasChannel ch = not . null . filter (messageOnChannel ch . snd)

-- | Does a track have events on a channel other than the given channel.
-- Note: This is not the opposite of `trackHasChannel`: Tracks may have events
-- on multiple channels, or no channels at all.
trackHasOtherChannel :: M.Channel -> M.Track a -> Bool
trackHasOtherChannel ch =
    not . null . filter (maybe False (/= ch) . messageChannel . snd)

-- | Is event on a given channel?
messageOnChannel :: M.Channel -> M.Message -> Bool
messageOnChannel ch = (== Just ch) . messageChannel

-- | Return the channel of an event, if any.
messageChannel :: M.Message -> Maybe M.Channel
messageChannel m = if M.isChannelMessage m then Just (M.channel m) else Nothing


prop_partitionGetsAll :: (Num a, Ord a) => [M.Track a] -> Bool
prop_partitionGetsAll =
    not . trackHasChannel 0 . concat . snd . partitionChannel 0

prop_partitionHasOnly :: (Num a, Ord a) => [M.Track a] -> Bool
prop_partitionHasOnly =
    not . trackHasOtherChannel 0 . fst . partitionChannel 0


extractChannel :: M.Channel -> M.Midi -> M.Track M.Ticks
extractChannel ch = fst . partitionChannel ch . tracks



type TrackChecker err = M.Track M.Time -> Checker err

checkChannel :: M.Channel -> TrackChecker err -> M.Midi -> CheckResult err
checkChannel ch chkr midi = execChecker $ chkr $ convert chTrack
  where
    chTrack = fst $ partitionChannel ch $ tracks midi
    convert = toRealTime (timeDiv midi) . toAbsTime

runCheckChannel :: (Show err) => M.Channel -> TrackChecker err -> M.Midi -> IO ()
runCheckChannel ch chkr midi = case checkChannel ch chkr midi of
    Left errs -> mapM_ print errs
    Right () -> print "PASSED"


processChannel :: (M.Track Time -> M.Track Time) -> M.Channel -> M.Midi -> M.Midi
processChannel f ch midi = midi { fileType = ft, tracks = f' chTrack : otherTracks }
  where
    (chTrack, otherTracks) = partitionChannel ch $ tracks midi
    ft = if null otherTracks then SingleTrack else MultiTrack
    f' = fromAbsTime . fromRealTime td . f . toRealTime td . toAbsTime
    td = timeDiv midi
