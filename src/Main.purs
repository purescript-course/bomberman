module Main
  ( Bomb
  , Bomberman
  , Enemy
  , BombermanId
  , Tile(..)
  , World
  , advanceTimers
  , bombLifespan
  , createInitialWorld
  , createReactor
  , draw
  , evenWallPlacement
  , explode
  , explosionLifespan
  , explosionRadius
  , handleEvent
  , height
  , isWall
  , main
  , move
  , place
  , placeBomb
  , playerMaxAmmo
  , shouldPlaceCrate
  , tick
  , width
  ) where

import Prelude

import Control.Monad.ST (ST)
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.ST (STArray)
import Data.Array.ST as STArray
import Data.Array.ST.Iterator (iterate, iterator)
import Data.Function (on)
import Data.Grid (Grid(..), Coordinates)
import Data.Grid as Grid
import Data.List (List, all)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Traversable (for, for_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Random (randomInt)
import Reactor (Reactor, Widget(..), executeDefaultBehavior, getW, modifyW_, runReactor, updateW, updateW_, withJust)
import Reactor.Events (Event(..))
import Reactor.Graphics.Colors as Color
import Reactor.Graphics.Drawing (Drawing, drawGrid, fill, tile)
import Reactor.Reaction (Reaction, ReactionM, widget)

width :: Int
width = 12

height :: Int
height = 12

playerMaxHp :: Int
playerMaxHp = 100

bombLifespan :: Int
bombLifespan = 3 * 60

explosionLifespan :: Int
explosionLifespan = 30

explosionRadius :: Int
explosionRadius = 4

playerMaxAmmo :: Int
playerMaxAmmo = 2

initialPlayerLocation :: Coordinates
initialPlayerLocation = { x: width / 2, y: height / 2 }

main :: Effect Unit
main = do
  reactor <- createReactor
  runReactor reactor
    { title: "Bomberman"
    , width
    , height
    , widgets:
        [ "section_hp" /\ Section { title: "Health" }
        , "label_hp" /\ Label { content: show $ playerMaxHp }
        , "section_score" /\ Section { title: "Score" }
        , "label_score" /\ Label { content: show 0 }
        ]
    }

data Tile
  = Wall
  | Crate
  | Empty
  | Bomb { timer :: Int }
  | Explosion { timer :: Int }

derive instance tileEq :: Eq Tile

data BombermanId = Player | Enemy Int

type Bomberman = { location :: Coordinates, ammo :: Int, hp :: Int }

type Enemy =
  { location :: Coordinates
  , ammo :: Int
  , lastDirection :: Maybe Coordinates
  , alive :: Boolean
  }

type Bomb = { timeRemaining :: Int, location :: Coordinates, placedBy :: BombermanId }

type World =
  { player :: Bomberman
  , enemies :: Array Enemy
  , board :: Grid Tile
  , time :: Int
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
  pure { player, enemies, board, time: 1 }
  where
  player =
    { location: initialPlayerLocation
    , ammo: playerMaxAmmo
    , hp: playerMaxHp
    }
  enemies = map createEnemy
    [ { x: width / 3, y: 2 * height / 3 }
    , { x: width / 3, y: 2 * height / 3 }
    , { x: width / 3, y: 2 * height / 3 }
    , { x: width / 3, y: 2 * height / 3 }
    ]
  createEnemy location =
    { location, ammo: playerMaxAmmo, lastDirection: Nothing, alive: true }
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
draw { player, board, enemies } = do
  drawGrid board drawTile
  withJust (Grid.index board player.location) \t ->
    fill (playerColorAt t) $ tile player.location
  for_ (Array.filter (_.alive) enemies) \{ location } ->
    fill (Color.pink400) $ tile location

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
    Tick _ -> do
      maybeHurtPlayer
      maybeHurtEnemies
      advanceTimers
    _ -> executeDefaultBehavior
  where
  movePlayer dir = do
    { player: p@{ location }, board, enemies } <- getW
    let bombermenLocations = Array.cons p.location (map (_.location) enemies)
    case move board bombermenLocations location dir of
      Just newLocation -> updateW_ { player: p { location = newLocation } }
      Nothing -> pure unit

move :: Grid Tile -> Array Coordinates -> Coordinates -> Coordinates -> Maybe Coordinates
move board bombermen { x, y } { x: xd, y: yd } =
  if hasSpace && notOccupied then Just newLocation else Nothing

  where
  newLocation = { x: x + xd, y: y + yd }
  hasSpace = Grid.index board newLocation == Just Empty
  notOccupied = all (\loc -> loc /= newLocation) bombermen

maybeHurtPlayer :: Reaction World
maybeHurtPlayer = do
  { player, board } <- getW
  case Grid.index board player.location of
    Just (Explosion _) -> do
      let nextHp = player.hp - 1
      if nextHp <= 0 then restartGame else updateW_ { player: player { hp = nextHp } }
    _ -> pure unit
  widget "label_hp" $ Label { content: show player.hp }

maybeHurtEnemies :: Reaction World
maybeHurtEnemies = do
  { board, enemies } <- getW
  let
    hurtEnemy enemy =
      case Grid.index board enemy.location of
        Just (Explosion _) -> enemy { alive = false }
        _ -> enemy
  updateW_ { enemies: map hurtEnemy enemies }
  log $ show $ Array.filter (not <<< _.alive) enemies
  widget "label_score" $
    Label { content: show $ Array.length $ Array.filter (not <<< _.alive) enemies }

restartGame :: Reaction World
restartGame = do
  newWorld <- liftEffect createInitialWorld
  modifyW_ $ const newWorld

advanceTimers :: Reaction World
advanceTimers = do
  { player, board: Grid tiles dimensions, time } <- getW
  updateW_ { time: time + 1 }
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
  when (time `mod` 15 == 0) do
    updateW_ { time: time + 1 }
    moveEnemies
  updateW_
    { board: Grid tiles' dimensions
    , player: player { ammo = playerMaxAmmo - activeBombs }
    }

  where
  isBomb (Bomb _) = true
  isBomb _ = false

moveEnemies :: Reaction World
moveEnemies = do
  { board, player, enemies } <- getW
  movedEnemies <- liftEffect $ for enemies \e -> do
    shouldMove <- (_ < 8) <$> randomInt 0 10
    let bombermenLocations = Array.cons player.location (map (_.location) enemies)
    maybeDirection <- choice
      $ Array.filter (isJust <<< move board bombermenLocations e.location)
      $ Array.filter (\d -> (opposite d <$> e.lastDirection) /= Just true) directions
    pure $
      if shouldMove then fromMaybe (e { lastDirection = Nothing }) do
        dir <- maybeDirection
        loc <- move board bombermenLocations e.location dir
        pure $ e { location = loc, lastDirection = Just $ dir }
      else e
  updateW_ { enemies: movedEnemies }

  where
  directions = [ { x: -1, y: 0 }, { x: 1, y: 0 }, { x: 0, y: 1 }, { x: 0, y: -1 } ]

opposite :: Coordinates -> Coordinates -> Boolean
opposite { x: -1, y: 0 } { x: 1, y: 0 } = true
opposite { x: 1, y: 0 } { x: -1, y: 0 } = true
opposite { x: 0, y: 1 } { x: 0, y: -1 } = true
opposite { x: 0, y: -1 } { x: 0, y: 1 } = true
opposite _ _ = false

choice :: forall a. Array a -> Effect (Maybe a)
choice [] = pure Nothing
choice xs = (xs !! _) <$> randomInt 0 (Array.length xs - 1)

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
