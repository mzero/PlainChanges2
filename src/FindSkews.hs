module FindSkews
    ( computeSkewsFromFiles
    )
  where

import qualified Codec.Wav as Wav
import Control.Monad (liftM2)
import qualified Data.Array.Unboxed as UA
import Data.Audio
import Data.Either (lefts)
import Data.Int (Int16)
import Text.Printf

-- | Find skews between two audio tracks of MechBass strings.
--
-- The two audio tracks should be recordings from playing the 'bassVolumeTest'
-- (see "Tests") on the MechBass. These recordings should be the G and D
-- strings, and the amplitude normalized. The code finds the start of sounding
-- for each note pair by finding the first large negative deflection. Then the
-- skew between them is computed.
--
-- The output is a set of lines of the form:
--    <fret from> -> <fret to> @ <offset in recording>   <ms 1> :: <ms 2> = <ms diff>
--
-- The code handles cases where there isn't a good recording and no note is
-- found. If so, the ms value is given as "x".
--
-- The offset to the first pair is, alas, given as a constant 'tOffset'.
-- Adjust as needed.
computeSkews :: Audio Int16 -> Audio Int16 -> [String]
computeSkews a1 a2 = concatMap timingTo [0..12]
  where
    timingTo t = map (timingToFrom t) [0..12]
    timingToFrom :: Int -> Int -> String
    timingToFrom t f =
        let tS = fromIntegral (14 * t + 1 + f) + tOffset
            tE = tS + 0.5
            f1 = findStart tS tE $ sampleData a1
            f2 = findStart tS tE $ sampleData a2
        in printf "%2d -> %2d  @ %5.1f   %4s :: %4s = %4s"
            f t tS (fmt f1) (fmt f2) (fmt $ liftM2 (-) f1 f2)

    fmt :: Maybe Double -> String
    fmt Nothing = "x"
    fmt (Just t) = printf "%3d" $ (round (1000 * t) :: Int)

    findStart :: Double -> Double -> UA.UArray Int Int16 -> Maybe Double
    findStart s e a  = case break id $ map (\i -> trigger > a UA.! i) [samp s..samp e] of
        (_,[]) -> Nothing
        (p,_) -> Just $ fromIntegral (length p) / sr

    sr = if sampleRate a1 == sampleRate a2
            then fromIntegral $ sampleRate a1
            else error "sample rates don't match"
    samp t = floor $ t * sr

    trigger = minBound `quot` 3
    tOffset = 1.5

-- | Compute skews from two WAV files. See 'computeSkews' for details.
computeSkewsFromFiles :: FilePath -> FilePath -> IO ()
computeSkewsFromFiles f1 f2 = do
    i1 <- Wav.importFile f1
    i2 <- Wav.importFile f2
    mapM_ print $ lefts [i1, i2]
    case (i1, i2) of
        (Right a1, Right a2) -> do
            mapM_ putStrLn $ computeSkews a1 a2
        _ -> return ()
