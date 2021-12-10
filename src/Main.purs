module Main where

import Prelude

import Data.Grid (Grid, Coordinates)
import Data.Grid as Grid
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Random (randomInt)
import Reactor (Reactor, executeDefaultBehavior, getW, runReactor, updateW_)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction)

width :: Int
width = 12

height :: Int
height = 12

bombLifespan :: Int
bombLifespan = 8

playerBombLimit :: Int
playerBombLimit = 2

main :: Effect Unit
main = runReactor reactor { title: "Bomberman", width, height }

data Tile
  = Wall
  | Crate
  | Empty

type Bomb = { timeRemaining :: Int, location :: Coordinates }

derive instance tileEq :: Eq Tile

type World =
  { player :: { location :: Coordinates, bombsRemaining :: Int }
  , board :: Grid Tile
  , bombs :: List Bomb
  }

reactor :: Reactor World
reactor = { initial, draw, handleEvent, isPaused: const true }

isBox :: Coordinates -> Effect Boolean
isBox { x, y } =
  (randomInt 1 3) >>= \prob ->
    if prob == 1 then
      pure true
    else
      pure false

isBox' :: Coordinates -> Effect Boolean
isBox' { x, y } = (_ == 1) <$> (randomInt 1 3)

initial :: World
initial = { player, board, bombs: Nil }
  where
  player =
    { location: { x: width / 2, y: height / 2 }
    , bombsRemaining: playerBombLimit
    }
  board =
    Grid.construct
      width
      height
      (\point -> if isWall point then Wall else Empty)

isWall :: Coordinates -> Boolean
isWall { x, y } =
  let
    isBorder = x == 0 || x == (width - 1) || y == 0 || y == (height - 1)
    shouldPlaceWall dim coord =
      if (dim `mod` 2 /= 0) then
        coord `mod` 2 == 0
      else
        evenWallPlacement (coord < (dim / 2)) coord
  in
    isBorder || (shouldPlaceWall width x && shouldPlaceWall height y)

evenWallPlacement :: Boolean -> Int -> Boolean
evenWallPlacement true currentIndex = currentIndex `mod` 2 == 0
evenWallPlacement _ currentIndex = (currentIndex - 1) `mod` 2 == 0

draw :: World -> Drawing
draw { player, board, bombs } = do
  drawGrid board drawTile
  for_ bombs \bomb ->
    fill (bombColor bomb.timeRemaining) $ tile bomb.location
  fill (playerColor player.location) $ tile player.location
  where
  drawTile Empty = Just Color.gray300
  drawTile Wall = Just Color.gray700
  drawTile Crate = Just Color.yellow900

  bombColor time = if time == 1 then Color.red500 else Color.yellow500
  playerColor loc =
    if List.elem loc $ map (\b -> b.location) bombs then Color.blue500
    else Color.blue400

handleEvent :: Event -> Reaction World
handleEvent event = do
  updateBombTimes
  case event of
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }
    KeyPress { key: " " } -> placeBomb
    _ -> executeDefaultBehavior

updateBombTimes :: Reaction World
updateBombTimes = do
  { bombs, player: player@{ bombsRemaining } } <- getW
  let
    tickedBombs = map tickBomb bombs
    notExplodedBombs = List.filter validBomb tickedBombs
    explodedCount = List.length tickedBombs - List.length notExplodedBombs
  updateW_
    { bombs: notExplodedBombs
    , player: player { bombsRemaining = bombsRemaining + explodedCount }
    }
  where
  -- Only update the timeRemaining field and leave the rest as it was
  tickBomb (bomb@{ timeRemaining }) = bomb { timeRemaining = timeRemaining - 1 }
  validBomb ({ timeRemaining }) = timeRemaining > 0

placeBomb :: Reaction World
placeBomb = do
  { player: player@{ location, bombsRemaining }, bombs } <- getW
  when (bombsRemaining > 0 && not (isBomb bombs location)) $ do
    let newBomb = { location, timeRemaining: bombLifespan }
    updateW_
      { bombs: (newBomb : bombs)
      , player: player { bombsRemaining = bombsRemaining - 1 }
      }

movePlayer :: { x :: Int, y :: Int } -> Reaction World
movePlayer { x: xd, y: yd } = do
  -- Match the patter {x, y} on player's location
  -- And also store the whole player in a variable by using player: player@(...)
  { player: player@({ location: { x, y } }), board, bombs } <- getW
  let newPlayerPosition = { x: x + xd, y: y + yd }
  when (isEmpty newPlayerPosition board && not (isBomb bombs newPlayerPosition)) $
    -- Update location in player by using: player { location = ... }
    updateW_ { player: player { location = newPlayerPosition } }
  where
  isEmpty position board = Grid.index board position == Just Empty

isBomb :: List Bomb -> Coordinates -> Boolean
isBomb bombs loc = loc `List.elem` map (\b -> b.location) bombs
