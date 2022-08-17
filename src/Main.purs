module Main where

import Prelude

import Contexts (Contexts)
import Data.Array (concat, length, mapWithIndex, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Hooks.UseClass (useClass)
import Jelly (Component, Signal, ch, chsFor, chsSig, el, launchApp, modifyAtom_, signal, useEventListener, (:=))
import Jelly.Hooks.UseMemo (useMemo)
import SlimeGame (Action(..), Direction(..), GameState(..), HorizontalDirection(..), SlimeSize(..), SlimeState, Tile(..), initState, stepState)
import Stages (stage1)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toEventTarget)
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent (fromEvent, key)

main :: Effect Unit
main = do
  launchApp root {}

root :: Component Contexts
root = el "div" do
  useClass $ pure "h-screen w-screen bg-gray-500"

  useClass $ pure "flex justify-center items-center"

  gameStateSig /\ gameStateAtom <- signal $ initState $ stage1

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
        "s" -> stepState $ ActionWait
        "z" -> stepState $ ActionUndo
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
    useClass $ pure "absolute rounded-xl transition-all duration-75"

    slimeSizeSig <- useMemo $ (\(_ /\ { size }) -> size) <$>
      slimeStateWithIndexSignal

    useClass $ do
      player <- playerSig
      i /\ _ <- slimeStateWithIndexSignal

      pure $ if player == i then "opacity-80" else "opacity-30"

    useClass $ do
      slimeSize <- slimeSizeSig
      case slimeSize of
        Small -> pure "h-[20px] w-[20px] bg-lime-600 opacity-30"
        Medium -> pure "h-[42px] w-[42px] bg-lime-600 opacity-30"
        Large -> pure "h-[62px] w-[62px] bg-lime-600 opacity-30"

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
    Wall -> useClass $ pure "bg-gray-500"
    Goal -> useClass $ pure "bg-cyan-200"
    VerticalRoad Small -> do
      ch $ el "div" do
        useClass $ pure "absolute h-[64px] w-[20px] bg-gray-500"
      ch $ el "div" do
        useClass $ pure "absolute h-[64px] w-[20px] bg-gray-500 left-[44px]"
    VerticalRoad Medium -> do
      ch $ el "div" do
        useClass $ pure "absolute h-[64px] w-[10px] bg-gray-500"
      ch $ el "div" do
        useClass $ pure "absolute h-[64px] w-[10px] bg-gray-500 left-[54px]"
    HorizontalRoad Small -> do
      ch $ el "div" do
        useClass $ pure "absolute h-[40px] w-[64px] bg-gray-500"
    HorizontalRoad Medium -> do
      ch $ el "div" do
        useClass $ pure "absolute h-[20px] w-[64px] bg-gray-500"
    _ -> pure unit
