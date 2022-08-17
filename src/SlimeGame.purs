module SlimeGame where

import Prelude

import Data.Array (any, elem, filter, findIndex, mapWithIndex, (!!))
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))

newtype GameState = GameState
  { slimes :: Array SlimeState
  , damagePositions :: Array (Int /\ Int)
  , player :: Int
  , prevState :: Maybe GameState
  , stage :: Stage
  }

derive instance Eq GameState

type SlimeState =
  { position :: Int /\ Int
  , size :: SlimeSize
  }

data SlimeSize = Small | Medium | Large

derive instance Eq SlimeSize
derive instance Ord SlimeSize

data HorizontalDirection = Left | Right

derive instance Eq HorizontalDirection

holDirToInt :: HorizontalDirection -> Int
holDirToInt Left = -1
holDirToInt Right = 1

data Direction = Horizontal HorizontalDirection | Upward HorizontalDirection

derive instance Eq Direction

type Stage =
  { tiles :: Array (Array Tile)
  , initSlimesState :: Array SlimeState
  , initDamagePositions :: Array (Int /\ Int)
  , initPlayer :: Int
  , title :: String
  , description :: String
  }

data Tile
  = Empty
  | Goal
  | Wall
  | VerticalRoad SlimeSize
  | HorizontalRoad SlimeSize

derive instance Eq Tile

data Action = ActionMove Direction | ActionWait | ActionUndo | ActionReset

derive instance Eq Action

initState :: Stage -> GameState
initState stage = GameState
  { slimes: stage.initSlimesState
  , player: stage.initPlayer
  , prevState: Nothing
  , stage
  , damagePositions: stage.initDamagePositions
  }

indexWithSlime :: GameState -> Int /\ Int -> Maybe Tile
indexWithSlime (GameState { slimes, stage: { tiles } }) (h /\ w) =
  let
    tile = (tiles !! h) >>= (_ !! w)
    isSlime = any (\s -> (s.position) == (h /\ w)) slimes
  in
    if isSlime then Nothing else tile

canProceed :: SlimeSize -> Tile -> Boolean
canProceed size tile = case tile of
  HorizontalRoad s -> s >= size
  Goal -> true
  Empty -> true
  _ -> false

computeEffect :: GameState -> GameState
computeEffect gameState@(GameState state) =
  let
    newSlimesFallen = map computeFalls state.slimes
    newSlimes = mapWithIndex computeDamage
      newSlimesFallen
    computeDamage i slime =
      let
        (h /\ w) = slime.position
        isDamaged = any (\(dh /\ dw) -> dh /\ dw == h /\ w)
          state.damagePositions
      in
        if isDamaged then case slime.size of
          Small -> fromMaybe slime $ state.stage.initSlimesState !! i
          Medium -> slime { size = Small }
          Large -> slime { size = Medium }
        else slime
    computeFalls slime =
      let
        under = (fst slime.position + 1) /\ snd slime.position
        tile = indexWithSlime gameState under
      in
        case tile of
          Just Empty -> computeFalls $ slime { position = under }
          Just (VerticalRoad s) | s >= slime.size -> computeFalls $ slime
            { position = under }
          Just Goal -> computeFalls $ slime
            { position = under }
          _ -> slime
    nowPlayerSize = case newSlimes !! state.player of
      Just slime -> slime.size
      Nothing -> Large
    newPlayer = case findIndex (\s -> s.size > nowPlayerSize) newSlimes of
      Just i -> i
      Nothing -> state.player
    newDamagePositions = filter
      ( \pos -> not $ any (\s -> s.position == pos)
          newSlimesFallen
      )
      state.damagePositions
  in
    GameState $ state
      { slimes = newSlimes
      , player = newPlayer
      , damagePositions = newDamagePositions
      }

stepState :: Action -> GameState -> GameState
stepState action gameState@(GameState state) = case action of
  ActionUndo -> fromMaybe gameState state.prevState
  ActionReset -> initState state.stage
  _ ->
    let
      newSlimes = case action of
        ActionMove (Horizontal dir) ->
          let
            playerSlime = fromMaybe { position: (0 /\ 0), size: Large } $
              (state.slimes !! state.player)
            targetPosition = (fst playerSlime.position) /\
              (snd playerSlime.position + holDirToInt dir)
            forwardSlimes = filter (\s -> s.position == targetPosition)
              state.slimes
            maxForwardSlimeSize = maximum $ map _.size forwardSlimes
            shouldEmptyPosition =
              if forwardSlimes == [] then targetPosition
              else fst targetPosition /\ (snd targetPosition + holDirToInt dir)
            shouldEmptySize = case maxForwardSlimeSize of
              Nothing -> playerSlime.size
              Just size -> size
            isEmpty =
              ( canProceed shouldEmptySize <$> indexWithSlime gameState
                  shouldEmptyPosition
              ) == Just true &&
                ( canProceed playerSlime.size <$>
                    ( (state.stage.tiles !! fst targetPosition) >>=
                        (_ !! snd targetPosition)
                    )
                ) == Just true
          in
            if isEmpty then
              mapWithIndex
                ( \i s ->
                    if i == state.player || elem s forwardSlimes then s
                      { position = (fst s.position) /\
                          (snd s.position + holDirToInt dir)
                      }
                    else s
                )
                state.slimes
            else state.slimes
        ActionMove (Upward dir) ->
          let
            movePlayer :: SlimeState -> SlimeState
            movePlayer slime =
              let
                throughPosition = (fst slime.position - 1) /\
                  (snd slime.position)
                targetPosition = (fst slime.position - 1) /\
                  (snd slime.position + holDirToInt dir)
                throughTile = indexWithSlime gameState throughPosition
                targetTile = indexWithSlime gameState targetPosition
                movedSlime = slime { position = targetPosition }
              in
                if
                  (canProceed slime.size <$> targetTile) == Just true &&
                    (canProceed slime.size <$> throughTile) == Just true then
                  movedSlime
                else slime
          in
            mapWithIndex (\i s -> if i == state.player then movePlayer s else s)
              state.slimes
        _ -> state.slimes
      gameStateMoved = GameState $ state
        { slimes = newSlimes, prevState = Just gameState }
    in
      computeEffect gameStateMoved

isClear :: GameState -> Boolean
isClear (GameState { slimes, stage: { tiles } }) = any
  ( \s ->
      let
        (h /\ w) = s.position
        tile = (tiles !! h) >>= (_ !! w)
      in
        tile == Just Goal
  )
  slimes
