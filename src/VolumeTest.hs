module VolumeTest (volumeTest) where

import Euterpea

import Elements

volumeTest :: Music Pitch
volumeTest = line $ map atVolume volumes
  where
    atVolume v =
        phrase [Dyn $ Loudness 95] (countOff v)
        :+: rest qn
        :+: phrase [Dyn $ Loudness $ toVel v] arppeg
        :+: rest qn

    volumes = [20,19..1]
    toVel = fromIntegral . (ceiling :: Double -> Int) . (/20) . (*128) . fromIntegral

    countOff v = onBassGString $ line $ concat $ zipWith (:)
        (replicate v $ note sn (G,4))
        (cycle [[], [], [rest sn]])

    arppeg = line $ map (\n -> transpose n strum) [7, 5, 0]
    strum = phrase [Art $ Legato 3.5 ] $ line
        [ onBassGString $ note en (G,3)
        , onBassDString $ note en (D,3)
        , onBassAString $ note en (A,2)
        , onBassEString $ note en (E,2)
        ]
