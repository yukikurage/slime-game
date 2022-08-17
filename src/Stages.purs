module Stages where

import Data.Tuple.Nested ((/\))
import SlimeGame (SlimeSize(..), Stage, Tile(..))

em :: Tile
em = Empty

go :: Tile
go = Goal

wa :: Tile
wa = Wall

vs :: Tile
vs = VerticalRoad Small

vm :: Tile
vm = VerticalRoad Medium

vl :: Tile
vl = VerticalRoad Large

hs :: Tile
hs = HorizontalRoad Small

hm :: Tile
hm = HorizontalRoad Medium

hl :: Tile
hl = HorizontalRoad Large

{-
type SlimeState =
  { position :: Int /\ Int
  , size :: SlimeSize
  , respawnPosition :: Int /\ Int
  }

-}

stage1 :: Stage
stage1 =
  { initPlayer: 0
  , initSlimesState:
      [ { position: 4 /\ 0
        , size: Large
        }
      , { position: 4 /\ 2
        , size: Large
        }
      ]
  , initDamagePositions:
      [ 4 /\ 5
      ]
  , tiles:
      [ [ em, em, em, em, em, em ]
      , [ em, em, em, em, em, em ]
      , [ em, em, em, em, em, em ]
      , [ em, em, em, em, em, go ]
      , [ em, em, em, em, em, em ]
      ]
  }
