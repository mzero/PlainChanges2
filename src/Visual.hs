module Visual where

{-
This module provides source material for creating visualations.

Alas, it is not directly derived from the score, and so, needs to be kept in
sync, as the score changes.
-}

import Data.List (intercalate, transpose)

import Changes

visualRing :: Show a => [a] -> [String]
visualRing as = map (intercalate " ") $ ringOf ps ++ [ps]
  where
    ts = map show as
    w = maximum $ map length ts
    ps = map pad ts
    pad s = s ++ replicate (w - length s) ' '


partIbass :: [String]
partIbass = intercalate [""] $ map visualRing $ starts ++ mids ++ finals
  where
    starts = map init mids
    mids = map tail finals
    finals =
        [ [G', E, C, G]
        , [Fs, B, G, D]
        , [D, A, Fs, B]
        , [G, D, B, E]
        ]

partIIbass :: [String]
partIIbass = beside $ matchHeight
    [ vr  0 b1pitches
    , vr  4 b2pitches
    , vr  8 b3pitches
    , vr 10 b4pitches
    ]
  where
    vr n p = padTop n $ visualRing p
    padTop n ts = pad n ts ++ ts
    padBottomTo n ts = ts ++ pad (n - length ts) ts
    pad n ts = replicate n $ replicate (length $ head ts) ' '
    matchHeight tts =
        let h = maximum $ map length tts
        in map (padBottomTo h) tts
    beside = map (intercalate "   ") . transpose

    b1pitches = [E, Df, Bf, G]
    b2pitches = [Bf, Af, F, C]
    b3pitches = [Df, Bf, Af, E]
    b4pitches = [F, Ef, C, G]

partIIIbass :: [String]
partIIIbass = visualRing [Bf', F, D, C, Bf]

plainChanges :: [String]
plainChanges =
    part "Part I" partIbass
    ++ part "Part II" partIIbass
    ++ part "PartIII" partIIIbass
  where
    part s ls = [s, ""] ++ ls ++ ["", "", ""]

writeVisual :: IO ()
writeVisual = putStr $ unlines plainChanges

-- | This is similar to the same named class in Euterpea, but it has a second
-- set (for octave above), and the Show instance converts with nice Unicode
-- music symbols.
data PitchClass  =  Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
                 |  Ef | Fff | Dss | E | Ff | Es | F | Gff | Ess | Fs
                 |  Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
                 |  Bf | Ass | B | Bs | Bss
                 |  Cff' | Cf' | C' | Dff' | Cs' | Df' | Css' | D' | Eff' | Ds'
                 |  Ef' | Fff' | Dss' | E' | Ff' | Es' | F' | Gff' | Ess' | Fs'
                 |  Gf' | Fss' | G' | Aff' | Gs' | Af' | Gss' | A' | Bff' | As'
                 |  Bf' | Ass' | B' | Bs' | Bss'
     deriving (Eq, Ord, Enum, Bounded)

instance Show PitchClass where
    show Cff = "C𝄫"
    show Cf = "C♭"
    show C = "C"
    show Dff = "D𝄫"
    show Cs = "C♯"
    show Df = "D♭"
    show Css = "C𝄪"
    show D = "D"
    show Eff = "E𝄫"
    show Ds = "D♯"
    show Ef = "E♭"
    show Fff = "F𝄫"
    show Dss = "D𝄪"
    show E = "E"
    show Ff = "F♭"
    show Es = "E♯"
    show F = "F"
    show Gff = "G𝄫"
    show Ess = "E𝄪"
    show Fs = "F♯"
    show Gf = "G♭"
    show Fss = "F𝄪"
    show G = "G"
    show Aff = "A𝄫"
    show Gs = "G♯"
    show Af = "A♭"
    show Gss = "G𝄪"
    show A = "A"
    show Bff = "B𝄫"
    show As = "A♯"
    show Bf = "B♭"
    show Ass = "A𝄪"
    show B = "B"
    show Bs = "B♯"
    show Bss = "B𝄪"
    show Cff' = "C⁸𝄫"
    show Cf' = "C⁸♭"
    show C' = "C⁸"
    show Dff' = "D⁸𝄫"
    show Cs' = "C⁸♯"
    show Df' = "D⁸♭"
    show Css' = "C⁸𝄪"
    show D' = "D⁸"
    show Eff' = "E⁸𝄫"
    show Ds' = "D⁸♯"
    show Ef' = "E⁸♭"
    show Fff' = "F⁸𝄫"
    show Dss' = "D⁸𝄪"
    show E' = "E⁸"
    show Ff' = "F⁸♭"
    show Es' = "E⁸♯"
    show F' = "F⁸"
    show Gff' = "G⁸𝄫"
    show Ess' = "E⁸𝄪"
    show Fs' = "F⁸♯"
    show Gf' = "G⁸♭"
    show Fss' = "F⁸𝄪"
    show G' = "G⁸"
    show Aff' = "A⁸𝄫"
    show Gs' = "G⁸♯"
    show Af' = "A⁸♭"
    show Gss' = "G⁸𝄪"
    show A' = "A⁸"
    show Bff' = "B⁸𝄫"
    show As' = "A⁸♯"
    show Bf' = "B⁸♭"
    show Ass' = "A⁸𝄪"
    show B' = "B⁸"
    show Bs' = "B⁸♯"
    show Bss' = "B⁸𝄪"

