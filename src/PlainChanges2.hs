module PlainChanges2 (plainChanges2_30) where

import Euterpea

import PartI
import PartII
import PartIII
import Preamble

plainChanges2_30 :: Music Pitch
plainChanges2_30 =
    rest hn
    :+: preamble
    :+: rest dwn
    :+: partI
    :+: rest dwn
    :+: partII
    :+: rest wn
    :+: partIII
    :+: rest (4*wn)
