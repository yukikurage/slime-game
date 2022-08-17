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

stages :: Array Stage
stages =
  [ stage1
  , tutorialStage1
  , tutorialStage2
  , tutorialStage3
  , tutorialStage4
  , tutorialFin
  , stage1
  ]

tutorialStage1 :: Stage
tutorialStage1 =
  { initPlayer: 0
  , initSlimesState:
      [ { position: 3 /\ 0
        , size: Large
        }
      ]
  , initDamagePositions:
      []
  , tiles:
      [ [ em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, em ]
      , [ em, em, em, wa, em, em, em ]
      , [ wa, wa, wa, wa, em, em, go ]
      ]
  , title: "チュートリアル 1: 操作"
  , description:
      "A: 左\nD: 右\nQ: 左上へジャンプ\nE: 右上へジャンプ\nZ: 一つ戻る\nR: リセット\n\nスライムは1段の段差を飛び越えることができます\n水色のゴールに到達すればクリアです。"
  }

tutorialStage2 :: Stage
tutorialStage2 =
  { initPlayer: 0
  , initSlimesState:
      [ { position: 1 /\ 0
        , size: Large
        }
      ]
  , initDamagePositions:
      [ 1 /\ 6, 4 /\ 0, 7 /\ 6 ]
  , tiles:
      [ [ em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, em ]
      , [ wa, wa, wa, vm, wa, wa, wa ]
      , [ em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, em ]
      , [ wa, wa, wa, vs, wa, wa, wa ]
      , [ em, em, em, em, em, em, em ]
      , [ go, em, em, em, em, em, em ]
      ]
  , title: "チュートリアル 2: 損傷"
  , description:
      "赤い障害物に当たるとダメージを受けます。\nダメージを受けるとスライムのサイズが小さくなります。\n小さくならないと通れないスペースが存在します。\n3回当たるとリスポーンします。"
  }

tutorialStage3 :: Stage
tutorialStage3 =
  { initPlayer: 0
  , initSlimesState:
      [ { position: 3 /\ 0
        , size: Large
        }
      , { position: 4 /\ 3
        , size: Large
        }
      ]
  , initDamagePositions:
      [ 3 /\ 6 ]
  , tiles:
      [ [ em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, go ]
      , [ em, em, em, em, em, em, em ]
      , [ wa, wa, wa, em, wa, wa, wa ]
      ]
  , title: "チュートリアル 3: 交代"
  , description:
      "スライムは複数現れる事もあります。あなたが操作できるのは、\"一番大きな\"スライムのみです。\n(正確には、自分より大きなスライムが現れたときに、そちらに操作が移ります。自分より大きなスライムが複数あるなら、スライムの番号(現在準備中)が一番若いものが優先されます。)"
  }

tutorialStage4 :: Stage
tutorialStage4 =
  { initPlayer: 0
  , initSlimesState:
      [ { position: 3 /\ 0
        , size: Large
        }
      , { position: 3 /\ 2
        , size: Large
        }
      , { position: 3 /\ 4
        , size: Large
        }
      ]
  , initDamagePositions:
      []
  , tiles:
      [ [ em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, em ]
      , [ em, em, em, em, wa, wa, em ]
      , [ em, em, em, em, em, em, go ]
      ]
  , title: "チュートリアル 4: 干渉"
  , description:
      "他のスライムを押し出すこともできます。\n ただし、一個までです。"
  }

tutorialFin :: Stage
tutorialFin =
  { initPlayer: 0
  , initSlimesState:
      [ { position: 2 /\ 0
        , size: Large
        }
      ]
  , initDamagePositions:
      []
  , tiles:
      [ [ em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, go ]
      ]
  , title: ""
  , description:
      "以上でチュートリアルは終わりです！いいスライム・ライフを。"
  }

stage1 :: Stage
stage1 =
  { initPlayer: 0
  , initSlimesState:
      [ { position: 3 /\ 6
        , size: Large
        }
      , { position: 3 /\ 4
        , size: Medium
        }
      , { position: 3 /\ 8
        , size: Small
        }
      ]
  , initDamagePositions:
      [ 9 /\ 10, 3 /\ 0, 9 /\ 2, 3 /\ 2, 3 /\ 1 ]
  , tiles:
      [ [ em, em, em, em, em, em, em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, em, em, em, em, em, em, em ]
      , [ wa, wa, wa, vs, wa, wa, wa, wa, wa, vm, wa, em, wa ]
      , [ em, em, em, em, wa, em, em, em, wa, em, em, em, em ]
      , [ em, em, em, em, hs, em, go, em, hs, em, em, em, em ]
      , [ em, em, em, em, wa, wa, wa, wa, wa, em, em, em, em ]
      , [ em, em, em, em, em, em, em, em, em, em, em, em, em ]
      , [ em, em, em, em, em, em, em, em, em, em, em, em, em ]
      ]
  , title: "ステージ 1: 隔離"
  , description:
      ""
  }
