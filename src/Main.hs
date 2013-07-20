module Main (main) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getProgName)
import System.FilePath ((</>))

import Euterpea (Music, Pitch)

import FindSkews
import PartI
import PartII
import PartIII
import PlainChanges2
import Play
import Preamble
import Tests
import Visual

parts :: [(String, Music Pitch)]
parts =
    [ ("whole",         plainChanges2_30)
    , ("preamble",      preamble)
    , ("partI",         partI)
    , ("partII",        partII)
    , ("partIII",       partIII)
    , ("levelTest",     levelTest)
    , ("bassVolume",    bassVolumeTest)
    , ("bassTiming",    bassTimingTest)
    ]

usage :: IO ()
usage = do
    progName <- getProgName
    putStr $ unlines $ map (sub "@prog@" progName) commands
    putStrLn $ "    " ++ intercalate " | " (map fst parts)
  where
    sub a b = intercalate b . splitOn a
    commands =
        [ "realtime output to synths:"
        , "    @prog@ play [<part>]                # play as composed on synths"
        , "    @prog@ perform [<part>]             # play as performed on synths"
        , "    @prog@ orch [<part>]                # play on orchestral instruments"
        , ""
        , "utilities:"
        , "    @prog@ midi <dir> [<part> | all]    # write midi files to a directory"
        , "    @prog@ visual                       # output text visualization of bass"
        , "    @prog@ skew <file-1> <file-2>       # compute bass skews between two audio files"
        , ""
        , "[<part>] defaults to the whole work, or is one of:"
        ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["play"]                -> playOnCoil whole
        ["play", p]             -> playOnCoil `withPart` p
        ["perform"]             -> performOnCoil whole
        ["perform", p]          -> performOnCoil `withPart` p
        ["orch"]                -> playOnOrch whole
        ["orch", p]             -> playOnOrch `withPart` p
        ["midi", dir]           -> midi dir "whole" whole
        ["midi", dir, "all"]    -> onAllNamedParts $ midi dir
        ["midi", dir, p]        -> midi dir `withNamedPart` p
        ["visual"]              -> writeVisual
        ["skew", f1, f2]        -> computeSkewsFromFiles f1 f2
        ["help"]                -> usage
        []                      -> usage
        _                       -> putStrLn "bad arguments" >> usage
    where
        onAllNamedParts act = mapM_ (uncurry act) parts
        act `withNamedPart` arg =
            maybe (partError arg) (act arg) $ lookup arg parts
        act `withPart` arg = const act `withNamedPart` arg

        whole = plainChanges2_30

        partError arg = putStrLn ("unknown part " ++ arg) >> usage

        midi dir name part = do
            createDirectoryIfMissing False dir
            prepareMidiFiles (dir </> name) part


