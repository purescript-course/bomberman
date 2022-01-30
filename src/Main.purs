module Main where

import Prelude

import Control.Monad.ST (ST)
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Array.ST.Iterator (iterate, iterator)
import Data.Grid (Grid(..), Coordinates)
import Data.Grid as Grid
import Data.List (List)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Random (randomInt)
import Reactor (Reactor, executeDefaultBehavior, getW, runReactor, updateW_, withJust)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction)

width :: Int
width = 12

height :: Int
height = 12

bombLifespan :: Int
bombLifespan = 3 * 60

explosionLifespan :: Int
explosionLifespan = 30

explosionRadius :: Int
explosionRadius = 4

playerMaxAmmo :: Int
playerMaxAmmo = 2

main :: Effect Unit
main = do
  reactor <- createReactor
  runReactor reactor { title: "Bomberman", width, height, widgets: [] }

data Tile
  = Wall
  | Crate
  | Empty
  | Bomb { timer :: Int }
  | Explosion { timer :: Int }

derive instance tileEq :: Eq Tile

type Bomb = { timeRemaining :: Int, location :: Coordinates }

type World =
  { player :: { location :: Coordinates, ammo :: Int }
  , board :: Grid Tile
  }

createReactor :: Effect (Reactor World)
createReactor = do
  initial <- createInitialWorld
  pure { initial, draw, handleEvent, isPaused: const false }

shouldPlaceCrate :: Effect Boolean
shouldPlaceCrate = (_ == 1) <$> (randomInt 1 3)

createInitialWorld :: Effect World
createInitialWorld = do
  board <- Grid.constructM width height constructor
  pure { player, board }
  where
  player =
    { location: { x: width / 2, y: height / 2 }
    , ammo: playerMaxAmmo
    }
  constructor point
    | isWall point = pure Wall
    | otherwise = do
        isCrate <- shouldPlaceCrate
        if isCrate then pure Crate else pure Empty

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
draw { player, board } = do
  drawGrid board drawTile
  withJust (Grid.index board player.location) \t ->
    fill (playerColorAt t) $ tile player.location
  where
  drawTile Empty = Just Color.gray300
  drawTile Wall = Just Color.gray700
  drawTile Crate = Just Color.yellow500
  drawTile (Explosion _) = Just Color.red800
  drawTile (Bomb { timer }) = Just $ bombColor timer

  bombColor time = if time < bombLifespan / 4 then Color.red800 else Color.red500

  playerColorAt (Bomb _) = Color.blue500
  playerColorAt _ = Color.blue400

handleEvent :: Event -> Reaction World
handleEvent event = do
  case event of
    KeyPress { key: "ArrowLeft" } -> movePlayer { x: -1, y: 0 }
    KeyPress { key: "ArrowRight" } -> movePlayer { x: 1, y: 0 }
    KeyPress { key: "ArrowDown" } -> movePlayer { x: 0, y: 1 }
    KeyPress { key: "ArrowUp" } -> movePlayer { x: 0, y: -1 }
    KeyPress { key: " " } -> placeBomb
    Tick _ -> advanceTimers
    _ -> executeDefaultBehavior

movePlayer :: { x :: Int, y :: Int } -> Reaction World
movePlayer { x: xd, y: yd } = do
  -- Match the patter {x, y} on player's location
  -- And also store the whole player in a variable by using player: player@(...)
  { player: player@({ location: { x, y } }), board } <- getW
  let newPlayerPosition = { x: x + xd, y: y + yd }
  when (isEmpty newPlayerPosition board) $
    -- Update location in player by using: player { location = ... }
    updateW_ { player: player { location = newPlayerPosition } }
  where
  isEmpty position board = Grid.index board position == Just Empty

advanceTimers :: Reaction World
advanceTimers = do
  { player: player, board: Grid tiles dimensions } <- getW
  let
    tiles' = STArray.run do
      -- Create a mutable copy of the array `tiles`
      arr <- STArray.thaw tiles
      -- Create an iterator of numbers 0..(length tiles - 1)
      iter <- iterator (\i -> map (const i) (tiles !! i))
      -- Iterate through the indices by calling (tick arr _) on each of them
      iterate iter $ tick arr
      -- Return the mutable array, now filled with updated tules
      pure arr
    activeBombs = Array.length (Array.filter isBomb tiles')
  updateW_
    { board: Grid tiles' dimensions
    , player: player { ammo = playerMaxAmmo - activeBombs }
    }
  where
  isBomb (Bomb _) = true
  isBomb _ = false

newExplosion :: Tile
newExplosion = Explosion { timer: explosionLifespan }

tick :: forall r. STArray r Tile -> Int -> ST r Unit
tick tiles here = do
  tile <- STArray.peek here tiles
  withJust tile case _ of
    -- If the timer of a bomb reaches 0, the bomb explodes
    Bomb { timer: 0 } -> explode tiles here
    Bomb { timer } -> placeHere $ Bomb { timer: timer - 1 }
    -- If the timer of an explosion reaches 0, the explosion disappears
    Explosion { timer: 0 } -> placeHere Empty
    Explosion { timer } -> placeHere $ Explosion { timer: timer - 1 }
    _ -> pure unit
  where
  placeHere = place tiles here

explode :: forall r. STArray r Tile -> Int -> ST r Unit
explode tiles origin = do
  place tiles origin newExplosion
  -- Akin to a for-each loop in OOP langugaes
  -- ...only here for_ is just a normal function that you could implement yourself
  for_ [ { x: 1, y: 0 }, { x: -1, y: 0 }, { x: 0, y: 1 }, { x: 0, y: -1 } ] \dir ->
    shockwave dir origin explosionRadius
  where
  to1D { x, y } = y * width + x
  to2D i = { x: i `mod` width, y: i / width }

  shockwave :: Coordinates -> Int -> Int -> ST r Unit
  shockwave _ _ 0 = pure unit
  shockwave dir here power = do
    next <- STArray.peek there tiles
    withJust next case _ of
      Wall -> pure unit
      Crate -> engulf
      Bomb _ -> explode tiles there
      _ -> engulf *> shockwave dir there (power - 1)
    where
    here' = to2D here
    there = to1D $ { x: here'.x + dir.x, y: here'.y + dir.y }
    engulf = place tiles there newExplosion

place :: forall r a. STArray r a -> Int -> a -> ST r Unit
place tiles i element =
  STArray.poke i element tiles $> unit

placeBomb :: Reaction World
placeBomb = do
  { player: player@{ location, ammo }, board } <- getW
  when (ammo > 0) $ do
    updateW_
      { player: player { ammo = ammo - 1 }
      , board: Grid.updateAt' location (Bomb { timer: bombLifespan }) board
      }
