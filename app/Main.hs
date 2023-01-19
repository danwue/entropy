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
import Entropy (Counter, Board, board, Move (Pick), GameState (AwaitingChaos, AwaitingRandom, AwaitingOrder), Square, handleInput, intsToSquare, makeTurn)
import Styles (grid_spacing)
import Foreign (moveBytes)
import GHC.Generics (URec(UAddr))
import Data.Bool (Bool (True))
import Data.Sequence (Seq(Empty))
import Control.Applicative (Applicative(pure))


-- | Type synonym for an application model

data Side = None | Order | Chaos deriving (Show, Eq)

data Difficulty = Ez | Md | Hd | NoDiff  deriving (Show, Eq)


data OrderCounter = Square (Char, Char) | Empty   deriving (Show, Eq)


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
  | GenerateCounter
  | Continue
  | MakeAiTurn
  | NewGame
  | PickSide Side
  | NoOp
  | Print
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
 randomSeed = 30,
 orderCounter = Main.Empty,
 gameProgress = Starting,
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
updateModel (PickSide Order) state = let new@GameInfo{..} = generateCounter state in noEff $ new{gs= makeAiTurn gs, side = Order, gameProgress=InProgress}
updateModel SayHelloWorld m = m <# do
  liftIO (print m) >> pure NoOp
updateModel (TakeCounter a b) state@GameInfo{..} = 
  case gs of 
    (AwaitingOrder board) -> noEff state{orderCounter = Square (intsToSquare (a,b))}
    _ -> noEff state

updateModel (PlaceCounter a b) state@GameInfo{..} =  
  case gs of
    (AwaitingChaos counter board) ->  if isLegalMove gs (Place counter (intsToSquare (a, b))) 
      then 
         state { gs = move gs (Place counter (intsToSquare(a,b))), orderCounter=Main.Empty }<# do liftIO (print state) >> pure MakeAiTurn  
      else state <# do liftIO (print state) >> pure NoOp
    _ -> noEff state 

updateModel MakeAiTurn state@GameInfo{..} = 
  case gs of
    (AwaitingRandom _) -> generateCounter state <# do liftIO (print state) >> pure MakeAiTurn   
    (AwaitingChaos counter board) -> state{gs= makeAiTurn gs} <# do liftIO (print state) >> pure NoOp   
    (AwaitingOrder  board) -> state{gs= makeAiTurn gs} <# do liftIO (print state) >> pure NoOp   

updateModel (Main.MoveCounter src a b) state@GameInfo{..} =  
  case gs of
    (AwaitingOrder board) ->  if isLegalMove gs (Move src (intsToSquare (a, b))) then noEff state{ gs= move gs (Move src (intsToSquare (a, b))), orderCounter=Main.Empty} else noEff state
    _ -> noEff state 
    
updateModel GenerateCounter state@GameInfo{..}= let new = generateCounter state in  noEff new
updateModel _ state = noEff state


makeAiTurn :: GameState -> GameState
makeAiTurn gs =  move gs (makeTurn gs)

isLegalMove :: GameState -> Move -> Bool
isLegalMove _ _= True

-- | Constructs a virtual DOM from a model
viewModel :: GameInfo -> View Action
viewModel model =  div_
    [class_ "container"]
    [ displayHeading model,  displayGame model]

displayGame :: GameInfo -> View Action
displayGame model@GameInfo {..}  =
  div_
    [style_ $ "position" =: "relative" <> "padding" =: S.ms(show grid_spacing ++"px")]
    [ displayTileContainer  model
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