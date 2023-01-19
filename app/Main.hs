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
import Styles (grid_spacing)
import Foreign (moveBytes)
import GHC.Generics (URec(UAddr))
import Data.Bool (Bool (True))
import Data.Sequence (Seq(Empty))
import Control.Applicative (Applicative(pure))
import Entropy (GameState(AwaitingOrder), validMoves)
import GHC.Base (absentErr)


-- | Type synonym for an application model

data Side = None | Order | Chaos deriving (Show, Eq)

data Difficulty = Ez | Md | Hd | NoDiff  deriving (Show, Eq)


data OrderCounter = Square (Char, Char) | Empty   deriving (Show, Eq)


data GameProgress
  = InProgress
  | Won
  | Starting
  deriving (Show, Eq)

-- | Sum type for application events
data Action
  = AddOne
  | PlaceCounter Int Int
  | GenerateCounter
  | ContinueScore
  | MakeAiTurn
  | PickSide Side
  | NoOp
  | ClearMessages
  | TakeCounter Int Int
  | MoveCounter Square Int Int
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
  orderCounter ::  OrderCounter,
  randomSeed :: Int,
  gameProgress :: GameProgress,
  message:: String
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
 gs = AwaitingChaos Nothing $ listArray (('A', 'a'), ('G', 'g')) $ repeat 0,
 message = "",
 randomSeed = 30,
 orderCounter = Main.Empty,
 gameProgress = Starting
}

counter:: GameState -> Maybe Counter
counter  (AwaitingChaos (Just counter) board)=  Just counter
counter _ = Nothing

getBoard:: GameInfo -> Board
getBoard  GameInfo{gs=gs} = Entropy.board(gs)

getRandomSeed:: GameInfo -> Int
getRandomSeed GameInfo{randomSeed=rs}= rs

getAllRemainingCounters :: Board -> [ CounterCount]
getAllRemainingCounters board =  Prelude.map (\el ->  (el , remainingCounters el board)) [1 .. 7]


generateCounter :: GameInfo -> GameInfo
generateCounter state@GameInfo{..} =
    let (el, seed) = giveRandomElement (Prelude.concat lists) (getRandomSeed state)
        in  state{randomSeed= seed, gs= move gs (Pick el)}
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
updateModel (PickSide Chaos) state@GameInfo{..} = generateCounter state{side=Chaos, gameProgress=InProgress} <# do liftIO (print state) >> pure NoOp
updateModel (PickSide Order) state = let new@GameInfo{..} = generateCounter state{message="AI is thinking",side = Order, gameProgress=InProgress} in new <# do liftIO (print new) >> pure MakeAiTurn 
updateModel SayHelloWorld m = m <# do
  liftIO (print m) >> pure NoOp
updateModel (TakeCounter a b) state@GameInfo{..} =
  case gs of
    (AwaitingOrder board) -> noEff state{orderCounter = Square (intsToSquare (a,b))}
    _ -> noEff state

updateModel (PlaceCounter a b) state@GameInfo{..} =
  case gs of
    (AwaitingChaos (Just counter) board) ->  if isLegalMove gs (Place counter (intsToSquare (a, b)))
      then
         state { gs = move gs (Place counter (intsToSquare(a,b))), orderCounter=Main.Empty, message= "AI is thinking" }<# do liftIO (print state) >> pure MakeAiTurn
      else state{message="Token already exists"} <# do liftIO (print state) >> pure NoOp
    _ -> noEff state

updateModel MakeAiTurn state@GameInfo{..} =
  case gs of
    (AwaitingChaos Nothing board) -> generateCounter state{message="AI is thinking"} <# do liftIO (print state) >> pure MakeAiTurn
    (AwaitingChaos (Just counter) board) -> state{gs= makeAiTurn gs} <# do liftIO (print state) >> pure ClearMessages
    (AwaitingOrder  board) -> state{gs= makeAiTurn gs} <# do liftIO (print state) >> pure GenerateCounter

updateModel (Main.MoveCounter src a b) state@GameInfo{..} =
  case gs of
    (AwaitingOrder board) ->  if isLegalMove gs (Move src (intsToSquare (a, b))) 
      then state{ gs= move gs (Move src (intsToSquare (a, b))), orderCounter=Main.Empty} <# do liftIO (print state)   >> pure GenerateCounter
      else noEff state{message="Invalid move", orderCounter=Main.Empty}
    _ -> noEff state

updateModel GenerateCounter state@GameInfo{..}= case side of
    Chaos ->  let new = generateCounter state{message=""} in  noEff new
    None ->   let new = generateCounter state{message=""} in  noEff new
    Order ->  let new = generateCounter state{message="AI is thinking"} in state <# do liftIO (print state)>> pure MakeAiTurn

updateModel ClearMessages state= noEff state{message=""} 

updateModel _ state = noEff state


makeAiTurn :: GameState -> GameState
makeAiTurn gs =  move gs (makeTurn gs)

isLegalMove :: GameState -> Move -> Bool
isLegalMove (AwaitingChaos (Just counter) board) (Place token (a, b)) =  (a, b) `elem` emptySquares  board
isLegalMove (AwaitingOrder board) (Move a b) = Prelude.or [a==b, b `elem` validMoves a board] 


-- | Constructs a virtual DOM from a model
viewModel :: GameInfo -> View Action
viewModel model =  div_
    [ style_ $ "width"=: "50%"<>"margin" =: "auto"]
    [ displayHeading model,  displayGame model]

displayGame :: GameInfo -> View Action
displayGame model@GameInfo {..}  =
  div_
    [style_ $  "padding" =: S.ms(show grid_spacing ++"px")]
    [ displayTileContainer  model]


displayTileContainer :: GameInfo -> View Action
displayTileContainer model@GameInfo{..} =
  div_
  [style_ $ "position" =: "absolute"<>
            "z-index" =: "2"<>
            "display" =:"flex"<>
            "max-width" =: S.ms(show field_width ++"px")<>
            "flex-wrap" =: "wrap"

  ]
  (concatMap (displayTile side orderCounter)  (tilesWithCoordinates seen_board))
  where
    seen_board = case orderCounter of
      Main.Empty -> board gs
      Square src ->  board gs // [(src,0)]




color :: Counter -> S.MisoString
color 1 = "#ffff00"
color 2 = "#ff00ff"
color 3 =  "#00ffff"
color 4 =   "#ff0000"
color 5 =  "#0000ff"
color 6 =  "#00ff00"
color 7 =  "#000000"
color _ ="#ffffff"


radius :: Counter -> S.MisoString
radius 0 = "0%"
radius _ ="50%"


displayTile :: Side -> OrderCounter -> (Counter, Int, Int) -> [View Action]
displayTile side oc (tile, a,b) =
  [ div_
      [style_ $
          "width"=: S.ms(show tile_size ++"px")<>
          "height"=:S.ms(show tile_size ++"px")<>
          "line-height"=: S.ms(show tile_size ++"px")<>
          "border-style" =:"solid"
      ] button
  ] where


    action = case (side, oc) of
      (Chaos, _)-> PlaceCounter a b
      (Order, Square sq) -> Main.MoveCounter sq a b
      (Order, Main.Empty) -> TakeCounter a b
      (_, _ ) -> NoOp


    button = [button_
                [
                  style_ $
                    "width"=: S.ms(show tile_size ++"px")<>
                    "height"=:S.ms(show tile_size ++"px")<>
                    "border-radius" =: radius tile<>
                    "line-height"=: S.ms(show tile_size ++"px")<>
                    "background-color"=: color tile,
                    onClick $ action
                ]
                (if tile>0 then [text  (S.ms $ show tile)] else [text "x"])
      ]


displayHeading :: GameInfo -> View Action
displayHeading model@GameInfo {..} =
  div_
    [ style_ $ "width"=: "50%"<>"text-align" =: "center"]
    [ h1_ [class_ "title"] [text "Entropy"]
    , div_
        [class_ "scores-container"]
        [ h3_
            [class_ "score-container"]
            [scoreDisplay, chooseSide, counterDisplay, messagesDisplay]
        ]
    ]
    where
      chooseSide = if gameProgress == Starting then div_[][button_[
                      style_ $
                        "margin-right" =:"10px"<>
                        "height"=:S.ms(show (tile_size-5) ++"px"),
                        onClick $ PickSide Chaos]
                      [text "Pick Chaos"],
                      button_
                      [style_ $
                        "height"=:S.ms(show (tile_size-5) ++"px"),
                        onClick $ PickSide Order]
                        [text "Pick Order"]
                        ] else text ""

      score = orderPoints $ board gs

      scoreDisplay = if gameProgress == InProgress then text $ S.ms ("Score(Chaos/Order): " ++  show (400 - score) ++ "/" ++  show score)  else text ""

      ctr= case counter gs of
        Nothing  -> div_[style_ $ "display"=:"inline-block"][text "-"]
        (Just x) ->  div_[style_ $
                      "width"=: S.ms(show tile_size ++"px")<>
                      "height"=:S.ms(show tile_size ++"px")<>
                      "border-radius" =: radius x<>
                      "line-height"=: S.ms(show tile_size ++"px")<>
                      "background-color"=: color x <>
                      "display"=:"inline-block"<>
                      "text-align" =: "center"
                     ][text $ S.ms ( show x)]


      counterDisplay = case side of
        None -> div_[][]
        _ -> div_[][text ("Random token:"),ctr]

      messagesDisplay = text (S.ms message)