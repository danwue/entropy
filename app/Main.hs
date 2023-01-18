-- | Haskell language pragma
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BlockArguments #-}

-- | Haskell module declaration
module Main where

import Styles
import Data.Array.Base (UArray (UArray))
import Data.Array.IArray
import qualified Data.Map as M
import           Entropy
import System.Random


-- | Miso framework import
import           Miso
import qualified Miso.String as S

-- | JSAddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import           Control.Monad.IO.Class
import Entropy (Counter, Board, board, Move (Pick), GameState (AwaitingChaos, AwaitingRandom), Square, handleInput, intsToSquare)
import Styles (grid_spacing)
import Foreign (moveBytes)
import GHC.Generics (URec(UAddr))


-- | Type synonym for an application model

data Side = None | Order | Chaos deriving (Show, Eq)

data Difficulty = Ez | Md | Hd | NoDiff  deriving (Show, Eq)


data GameProgress
  = InProgress
  | Continuing
  | GameOver
  | Won
  | Starting
  deriving (Show, Eq)

-- | Sum type for application events
data Action
  = AddOne
  | PlaceCounter Int Int
  | SubtractOne
  | Continue
  | NewGame
  | TileClicked (Int, Int)
  | PickSide Side
  | NoOp
  | Print
  | SayHelloWorld
  deriving (Eq, Show)

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f = JSaddle.debugOr 8080 (f >> syncPoint) JSaddle.jsaddleApp
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
type CounterCount = (Counter, Int)

data GameInfo = GameInfo {
  side :: Side,
  gs :: GameState,
  randomSeed :: Int,
  gameProgress :: GameProgress,
  score :: Int,
  difficulty :: Difficulty
} deriving ( Eq, Show )


main :: IO ()
main = do
  stdGen <- getStdGen
  let (seed, _) = random stdGen
  runApp $ startApp App {model = initialModel {randomSeed = seed}, ..}
  where
    initialAction = SayHelloWorld -- initial action to be executed on application load
    model  = initialModel              -- initial model
    update = updateModel          -- update function
    view   = viewModel            -- view function
    events = defaultEvents        -- default delegated events
    subs   = []                   -- empty subscription list
    mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in synch (only used with `miso` function)


initialModel:: GameInfo
initialModel = GameInfo{side = None,
 gs = AwaitingRandom $ listArray (('A', 'a'), ('G', 'g')) $ repeat 0,
 difficulty = NoDiff,
 randomSeed=30,
 gameProgress=Starting,
 score=0
 }

counter:: GameState -> Maybe Counter
counter  (AwaitingChaos counter board)=  Just counter
counter _ = Nothing

getBoard:: GameInfo -> Board
getBoard  GameInfo{gs=gs} = Entropy.board(gs)


getRandomSeed:: GameInfo -> Int
getRandomSeed GameInfo{randomSeed=rs}= rs

getAllRemainingCounters :: Board -> [ CounterCount]
getAllRemainingCounters board =  Prelude.map (\el ->  (el , remainingCounters el board)) [1 .. 7]


generateCounter :: GameInfo -> (GameInfo, Counter)
generateCounter state =
    let (el, seed) = giveRandomElement (Prelude.concat lists) (getRandomSeed state)
        in  (state{randomSeed= seed}, el)
    where
       lists = Prelude.map(\pair -> Prelude.replicate (snd pair) (fst pair)) (getAllRemainingCounters $ getBoard state)

giveRandomElement :: [a] -> Int -> (a, Int)
giveRandomElement ls gen =  (el, nseed) where
        grt  = mkStdGen gen
        (n, newGen) = randomR (0, Prelude.length ls - 1) grt
        (nseed, _) = random newGen
        el = ls !! n




-- | Updates model, optionally introduces side effects

updateModel :: Action -> GameInfo-> Effect Action GameInfo
updateModel (PickSide Chaos) state@GameInfo{..} = let (new, ctr) = generateCounter state in  new{side = Order, gameProgress=InProgress, gs = AwaitingChaos  ctr (board gs)} <# do liftIO (print new) >> pure NoOp
updateModel (PickSide Order) state@GameInfo{..} = let (new, ctr) = generateCounter state in  new{side = Order, gameProgress=InProgress, gs = AwaitingChaos  ctr (board gs)} <# do liftIO (print new) >> pure NoOp
updateModel SayHelloWorld m = m <# do
  liftIO (print m) >> pure NoOp
updateModel (PlaceCounter a b) state@GameInfo{..} =  
  case gs of
    (AwaitingRandom _)  -> noEff state
    (AwaitingChaos counter board) -> noEff state{gs= AwaitingOrder $ board // [(intsToSquare (a, b), counter)]}
    (AwaitingOrder _) -> let (new, ctr) = generateCounter state in  new{side = Order, gameProgress=InProgress, gs = AwaitingChaos  ctr (board gs)} <# do liftIO (print new) >> pure NoOp
updateModel _ m =  noEff m




-- | Constructs a virtual DOM from a model
viewModel :: GameInfo -> View Action
viewModel model =  div_
    [class_ "container"]
    [ displayHeading model,  displayGame model]


gridRow :: View Action
gridRow = div_ [class_ "grid-row"] (replicate 7 gridCell)
  where
    gridCell = div_[
      style_ $ "width" =: S.ms(show tile_size ++"px") <>
              "height" =: S.ms(show tile_size ++"px") <>
              "float" =: "left"<>
              "border-style" =:"solid"
      ][]

displayContainer :: View Action
displayContainer =
  div_
    [style_ $ "position" =: "absolute" <> "z-index" =: "1"]
    (replicate 7 gridRow)

displayGame :: GameInfo -> View Action
displayGame model@GameInfo {..}  =
  div_
    [style_ $ "position" =: "relative" <> "padding" =: S.ms(show grid_spacing ++"px")]
    [displayContainer
    , displayTileContainer  model
    ]


displayTileContainer :: GameInfo -> View Action
displayTileContainer model@GameInfo{..} =
  div_
    [style_ $ "position" =: "absolute"<>
              "z-index" =: "2"<>
              "display" =:"flex"<>
              "max-width" =: S.ms(show field_width ++"px")<>
              "flex-wrap" =: "wrap"

    ]
    (concatMap (displayTile side)  (tilesWithCoordinates (board gs)))


color :: Counter -> S.MisoString
color 1 = "#ffff00"
color 2 = "#ff00ff"
color 3 =  "#00ffff"
color 4 =   "#ff0000"
color 5 =  "#0000ff"
color 6 =  "#00ff00"
color 7 =  "#000000"
color _ ="#ffffff"

displayTile :: Side -> (Counter, Int, Int) -> [View Action]
displayTile side (tile, a,b) =
  [ div_
      [style_ $
          "width"=: S.ms(show tile_size ++"px")<>
          "height"=:S.ms(show tile_size ++"px")<>
          "line-height"=: S.ms(show tile_size ++"px")<>
          "margin-left"=:"10px"<>
          "margin-top"=:"5px"
      ] button
  ] where 

    action = PlaceCounter
    button = [button_ 
                [
                  style_ $
                    "width"=: S.ms(show tile_size ++"px")<>
                    "height"=:S.ms(show tile_size ++"px")<>
                    "border-radius" =: "50%"<>
                    "line-height"=: S.ms(show tile_size ++"px")<>
                    "margin-left"=: S.ms (show "10px")<>
                    "margin-top"=:S.ms ( show "5px")<>
                    "background-color"=: color tile,
                    onClick $ PlaceCounter a b
                ]
                [text  (S.ms $ show tile)]
      ]
    



displayHeading :: GameInfo -> View Action
displayHeading model@GameInfo {..} =
  div_
    [class_ "heading"]
    [ h1_ [class_ "title"] [text "Entropy"]
    , div_
        [class_ "scores-container"]
        [ div_
            [class_ "score-container"]
            [scoreDisplay, chooseSide, counterDisplay]
        ]
    ]
    where  
      chooseSide = if gameProgress == Starting then div_[][button_[onClick $ PickSide Chaos][text "Pick Chaos"], button_[onClick $ PickSide Order][text "Pick Order"]] else text ""
      scoreDisplay = if gameProgress == InProgress then text $ S.ms $ show score else text ""
      counterDisplay = Prelude.maybe (div_[][]) (\x->(div_[][text $ S.ms (show x)])) (counter gs)