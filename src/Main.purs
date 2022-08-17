module Main where

import Prelude

import Contexts (Contexts)
import Data.Array (concat, length, mapWithIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Hooks.UseClass (useClass)
import Jelly (Component, Signal, ch, chsFor, chsSig, el, launchApp, modifyAtom_, signal, text, useEventListener, useSignal, writeAtom, (:=))
import Jelly.Hooks.UseMemo (useMemo)
import SlimeGame (Action(..), Direction(..), GameState(..), HorizontalDirection(..), SlimeSize(..), SlimeState, Tile(..), initState, isClear, stepState)
import Stages (stages, tutorialStage1)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (fromEvent, key)

main :: Effect Unit
main = do
  launchApp root {}

root :: Component Contexts
root = el "div" do
  useClass $ pure "h-screen w-screen bg-gray-600"

  useClass $ pure "flex flex-col justify-center items-center gap-6"

  stageNumSig /\ stageNumAtom <- signal 0

  gameStateSig /\ gameStateAtom <- signal $ initState tutorialStage1

  isAllClearSig <- useMemo $ (_ >= length stages) <$> stageNumSig

  useSignal do
    gameState <- gameStateSig
    if isClear gameState then
      liftEffect $ modifyAtom_ stageNumAtom $ \n -> n + 1
    else
      pure unit

  useSignal do
    stageNum <- stageNumSig
    case stages !! stageNum of
      Just stage -> liftEffect $ writeAtom gameStateAtom $ initState stage
      Nothing -> pure unit

  heightSig <- useMemo $ (\(GameState { stage: { tiles } }) -> length tiles) <$>
    gameStateSig
  widthSig <-
    useMemo $
      ( \(GameState { stage: { tiles } }) -> fromMaybe 0 $ length <$>
          (tiles !! 0)
      ) <$>
        gameStateSig
  stageSig <- useMemo $ (\(GameState { stage }) -> stage) <$> gameStateSig
  slimesWithIndexSig <- useMemo $
    (\(GameState { slimes }) -> mapWithIndex (\i s -> i /\ s) slimes) <$>
      gameStateSig
  playerSig <- useMemo $ (\(GameState { player }) -> player) <$> gameStateSig
  damagePositionsSig <- useMemo $
    (\(GameState { damagePositions }) -> damagePositions) <$> gameStateSig

  htmlDocument <- liftEffect $ document =<< window

  let
    listener e = case fromEvent e of
      Just ke -> modifyAtom_ gameStateAtom case key ke of
        "q" -> stepState $ ActionMove $ Upward $ Left
        "e" -> stepState $ ActionMove $ Upward $ Right
        "a" -> stepState $ ActionMove $ Horizontal $ Left
        "d" -> stepState $ ActionMove $ Horizontal $ Right
        "z" -> stepState $ ActionUndo
        "r" -> stepState $ ActionReset
        _ -> identity
      Nothing -> pure unit

  useEventListener "keydown" listener (toEventTarget htmlDocument)

  ch $ el "div" do

    useClass $ pure "relative bg-white"

    "style" :=
      ( \h w -> "height: " <> show (h * 64) <> "px; width: " <> show (w * 64) <>
          "px;"
      )
      <$> heightSig
      <*>
        widthSig

    chsSig do
      { tiles } <- stageSig
      pure $ concat $ mapWithIndex
        (\i row -> mapWithIndex (tileComponent i) row)
        tiles

    chsSig do
      damagePositions <- damagePositionsSig
      pure $ map damagePositionComponent damagePositions

    chsFor slimesWithIndexSig (\(i /\ _) -> Just $ show i) \slimeSignal ->
      slimeComponent slimeSignal playerSig

  ch $ el "div" do
    useClass $ pure "text-xl text-white"

    ch $ text $ (\(GameState { stage: { title } }) -> title) <$>
      gameStateSig

  ch $ el "pre" do
    useClass $ pure "text-md text-white"

    ch $ text $ (\(GameState { stage: { description } }) -> description) <$>
      gameStateSig

slimeComponent
  :: (Signal (Int /\ SlimeState)) -> Signal Int -> Component Contexts
slimeComponent slimeStateWithIndexSignal playerSig = el "div" $ do
  useClass $ pure "absolute transition-all duration-75"

  "style" :=
    ( \(_ /\ { position: h /\ w }) ->
        "top: " <> show (h * 64) <> "px; left: " <> show (w * 64) <> "px;"
    ) <$> slimeStateWithIndexSignal

  useClass $ pure "h-[64px] w-[64px]"
  useClass $ pure "flex justify-center items-end"

  ch $ el "div" do
    useClass $ pure "absolute transition-all duration-75"

    slimeSizeSig <- useMemo $ (\(_ /\ { size }) -> size) <$>
      slimeStateWithIndexSignal

    useClass $ do
      player <- playerSig
      i /\ _ <- slimeStateWithIndexSignal

      pure $ if player == i then "opacity-80" else "opacity-30"

    useClass $ do
      slimeSize <- slimeSizeSig
      case slimeSize of
        Small -> pure "h-[20px] w-[20px] bg-lime-600 rounded-sm"
        Medium -> pure "h-[42px] w-[42px] bg-lime-600 rounded-md"
        Large -> pure "h-[62px] w-[62px] bg-lime-600 rounded-xl"

damagePositionComponent
  :: Int /\ Int -> Component Contexts
damagePositionComponent (h /\ w) = el "div" $ do
  useClass $ pure "absolute transition-all duration-75"

  "style" := pure
    ("top: " <> show (h * 64) <> "px; left: " <> show (w * 64) <> "px;")

  useClass $ pure "h-[64px] w-[64px]"
  useClass $ pure "flex justify-center items-center"

  ch $ el "div" do
    useClass $ pure "absolute rounded-md transition-all duration-75"

    useClass $ pure "h-[50px] w-[50px] bg-red-200"

tileComponent :: Int -> Int -> Tile -> Component Contexts
tileComponent h w tile = el "div" do
  "style" := pure
    ( "position: absolute; top: " <> show (h * 64) <> "px; left: "
        <> show (w * 64)
        <>
          "px;"
    )

  useClass $ pure "h-[64px] w-[64px]"

  case tile of
    Wall -> useClass $ pure "bg-gray-600"
    Goal -> useClass $ pure "bg-cyan-200"
    VerticalRoad Small -> do
      ch $ el "div" do
        useClass $ pure "absolute h-[64px] w-[20px] bg-gray-600"
      ch $ el "div" do
        useClass $ pure "absolute h-[64px] w-[20px] bg-gray-600 left-[44px]"
    VerticalRoad Medium -> do
      ch $ el "div" do
        useClass $ pure "absolute h-[64px] w-[10px] bg-gray-600"
      ch $ el "div" do
        useClass $ pure "absolute h-[64px] w-[10px] bg-gray-600 left-[54px]"
    HorizontalRoad Small -> do
      ch $ el "div" do
        useClass $ pure "absolute h-[40px] w-[64px] bg-gray-600"
    HorizontalRoad Medium -> do
      ch $ el "div" do
        useClass $ pure "absolute h-[20px] w-[64px] bg-gray-600"
    _ -> pure unit
