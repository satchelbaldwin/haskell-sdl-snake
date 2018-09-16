{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Foldable
import Data.Time.Clock.System
import Lens.Micro
import Lens.Micro.Platform
import qualified SDL
import qualified SDL.Font as SDF
import SDL.Vect
import Foreign.C.Types
import Math.Geometry.Grid
import Math.Geometry.GridMap.Lazy
import qualified Math.Geometry.GridMap as GM
import Math.Geometry.Grid.Square
import Data.List
import qualified Data.Text as T
import GHC.Word
import System.IO.Unsafe
import System.Random -- use of randomRIO in placing food

data TileType = Player | Empty | Wall | Food deriving (Eq, Show)

data MoveDirection = North | South | East | West deriving (Eq, Show)

type GameBoard = LGridMap TorSquareGrid TileType

data GameSize = GameSize Int Int Int deriving (Show)
data GameState = GameState
  { _gameBoard :: GameBoard
  , _gameSize  :: GameSize
  , _lastDir   :: MoveDirection
  , _moveTimer :: Float
  , _moveSpeed :: Float
  , _playerLoc :: (Int, Int)
  , _pSize      :: Int 
  , _pastMoves :: [MoveDirection]
  , _gameOver  :: Bool
  } deriving (Show)

makeLenses ''GameState

data Input = Input 
  { _keyUp      :: Bool
  , _keyLeft    :: Bool
  , _keyRight   :: Bool
  , _keyDown    :: Bool
  , _keyRestart :: Bool
  , _quitEvent  :: Bool
  } deriving (Show)

makeLenses ''Input

flipDirection :: MoveDirection -> MoveDirection
flipDirection d = case d of
  North -> South
  South -> North
  East  -> West
  West  -> East

applyDirection :: MoveDirection -> (Int, Int) -> (Int, Int)
applyDirection dir (x, y) = 
  case dir of 
    North -> (  x, y-1)
    South -> (  x, y+1)
    East  -> (x+1, y  )
    West  -> (x-1, y  )

initSDL :: Int -> Int -> IO (SDL.Window, SDL.Renderer)
initSDL screenWidth screenHeight = do
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow
    "SDL2"
    SDL.defaultWindow
    {SDL.windowInitialSize = 
      V2 (fromIntegral screenWidth  )
         (fromIntegral screenHeight )}
  renderer <- SDL.createRenderer
    window
    (-1)
    SDL.defaultRenderer
  SDL.showWindow window
  return (window, renderer)

initSDLFont :: IO SDF.Font
initSDLFont = do
  SDF.initialize
  SDF.load "/usr/share/fonts/TTF/DejaVuSans.ttf" 32 

drawSDLText :: 
  SDL.Window -> 
  SDL.Renderer -> 
  SDF.Font ->
  Int ->
  Int ->
  V4 Word8 ->
  T.Text ->
  IO ()
drawSDLText window renderer font x y color contents = do
  fontSurface <- SDF.blended font color contents 
  (textWidth, textHeight) <- SDF.size font contents
  let size    = (V2 (fromIntegral textWidth) (fromIntegral textHeight))
  let source  = (P $ V2 (fromIntegral 0) (fromIntegral 0))
  let dest    = (P $ V2 (fromIntegral x) (fromIntegral y))
  let sourceR = Just $ SDL.Rectangle origin size
  let destR   = Just $ SDL.Rectangle dest   size
  texture <- SDL.createTextureFromSurface renderer fontSurface
  _ <- SDL.copy renderer texture sourceR destR
  
  SDL.destroyTexture texture
  SDL.freeSurface fontSurface
  return ()

quitSDL :: SDL.Window -> IO ()
quitSDL window = do
  SDL.destroyWindow window
  SDL.quit

getInput :: IO Input
getInput = 
  let up      = SDL.ScancodeW
      right   = SDL.ScancodeD
      left    = SDL.ScancodeA
      down    = SDL.ScancodeS
      restart = SDL.ScancodeR
      escape  = SDL.ScancodeEscape
  in do
   events <- map SDL.eventPayload <$> SDL.pollEvents
   keymap <- SDL.getKeyboardState
   return $ 
    Input { _keyUp      = keymap up
          , _keyRight   = keymap right
          , _keyLeft    = keymap left
          , _keyDown    = keymap down
          , _keyRestart = keymap restart
          , _quitEvent  = keymap escape
          }

getTime :: IO Float
getTime = 
  liftA2 (/) 
    (fromIntegral . systemNanoseconds <$> getSystemTime)
    (pure 1e9 :: IO Float)

flipTimeOverflow :: Float -> IO (Float, Float)
flipTimeOverflow pastTime = do
  currentTime <- getTime
  let difference = currentTime - pastTime
  let adjusted   = (if difference > 0 then 0.0 else 1.0) + difference
  return $ (adjusted, currentTime)

mainLoop :: 
  SDL.Window -> 
  SDL.Renderer ->
  SDF.Font ->
  GameState -> 
  Float -> 
  IO ()
mainLoop window renderer font state prevTime = do
  input <- getInput
  (dt, time) <- flipTimeOverflow prevTime
  let state' = if (state^.gameOver == False)
      then state
           & setPlayerDirection input
           & gameLogic input dt
      else state
           & waitForRestart input
  render window renderer font state'
  SDL.present renderer
  unless (input^.quitEvent) (mainLoop window renderer font state' time)

gameLogic input dt state =
  state & placeFood 
        & playerMove
        & collision 
        & changeTiles
  where
    playerMove :: GameState -> GameState
    playerMove s = 
      if s^.moveTimer > s^.moveSpeed
        then let d = (s^.lastDir) in
          s & playerLoc %~ applyDirection d 
            & pastMoves %~ ((:) d) 
            & moveTimer .~ 0
            & moveSpeed %~ getSpeed
        else s & moveTimer %~ (+ dt)
      where 
        getSpeed old = old - ((old - 0.1) / 10.0)
    collision :: GameState -> GameState
    collision s =
      if s^.moveTimer == 0 -- check collision only if player has just moved
      then
        case (s^.gameBoard) GM.! (s^.playerLoc) of
          Empty  -> s
          Wall   -> s & gameOver .~ True
          Player -> s & gameOver .~ True
          Food   -> s & pSize %~ (+ 1)
                      & gameBoard %~ (GM.adjust (const Empty) (s^.playerLoc))
      else s
    changeTiles :: GameState -> GameState
    changeTiles s = 
        (gameBoard .~ 
          ((GM.adjust (const Player) (s^.playerLoc))
          (stepF 
            (cutPlayerTiles s) 
            (take ((s^.pSize) - 1) $ s^.pastMoves) 
            (s^.playerLoc)))
        ) (s) -- note: refactor to `over` rather than `set`?
      where
        stepF :: GameState -> [MoveDirection] -> (Int, Int) -> GameBoard
        stepF t (d:ds) (x,y) =
          let adj = applyDirection (flipDirection d) (x,y)
          in GM.adjust (const Player) adj $ stepF t ds adj
        stepF t _ (x,y) = t^.gameBoard
        cutPlayerTiles :: GameState -> GameState
        cutPlayerTiles t = t & gameBoard %~ (GM.map playerToEmpty)
        playerToEmpty :: TileType -> TileType
        playerToEmpty Player = Empty
        playerToEmpty t = t
    placeFood :: GameState -> GameState
    placeFood s = 
        if (numFood < 3) 
          then placeTile (placeTile s randomNonPlayerTile) randomNonPlayerTile
          else s
      where
        placeTile s (x, y) =
          s & gameBoard %~ GM.adjust (const Food) (x, y) 
        randomNonPlayerTile :: (Int, Int)
        randomNonPlayerTile = do
          let rx = unsafePerformIO $ randomRIO (1, w-2) 
          let ry = unsafePerformIO $ randomRIO (1, h-2)
          if ((s ^. gameBoard) GM.! (rx, ry)) == Player
            then
              randomNonPlayerTile
            else
              (rx, ry)
          where
            (w, h) = (size (s^.gameBoard))
        numFood = length 
                  . filter (== Food) 
                  . map snd 
                  . GM.toList 
                  $ (s^.gameBoard) 
      
waitForRestart input state =
  if input^.keyRestart 
    then initialState 
      (width  $ state^.gameSize) 
      (height $ state^.gameSize)
      (tiles  $ state^.gameSize)
    else state
  where
    width  (GameSize w _ _) = w
    height (GameSize _ h _) = h
    tiles  (GameSize _ _ t) = t

setPlayerDirection input state = 
  state & lastDir %~ direction'
  where
    direction' lastDirection =
      case findIndex ((==) True) inputs of
           Just i  -> [North, South, West, East] !! i
           Nothing -> lastDirection
    inputs = [input^.keyUp, input^.keyDown, input^.keyLeft, input^.keyRight]

initialState :: Int -> Int -> Int -> GameState
initialState screenWidth screenHeight tileSize = GameState 
  { _gameBoard = lazyGridMapIndexed 
      (torSquareGrid width height) 
      (mkWalls width height)
  , _gameSize  = GameSize screenWidth screenHeight tileSize
  , _lastDir   = South
  , _moveTimer = 0.0
  , _moveSpeed = 0.6
  , _playerLoc = (width `div` 2, height `div` 2) -- centerTile
  , _pSize     = 1
  , _pastMoves = []
  , _gameOver  = False
  } & gameBoard %~ (\g -> GM.adjust (const Player) centerTile g)
  where
    width  = screenWidth  `div` tileSize
    height = screenHeight `div` tileSize 
    centerTile = over both (div 2) (width, height)
    mkWalls w h = 
      map
      (\(x,y) -> if x == 0 
                 || y == 0
                 || x == w-1
                 || y == h-1
                 then ((x,y), Wall)
                 else ((x,y), Empty)) 
      [ (xc,yc) | xc <- [0..w], yc <- [0..h] ]

render :: SDL.Window -> SDL.Renderer -> SDF.Font -> GameState -> IO ()
render window renderer font state =
  if (state^.gameOver == False)
    then do
      mapM_ (drawTile renderer) (GM.toList $ state^.gameBoard)
      drawText 50 50 (V4 255 255 255 255) (T.pack . show $ state^.pSize)
    else renderGameOverText $ state^.pSize
  where
    gT :: GameSize -> Int
    gT (GameSize _ _ ts) = ts
    tileToSDL :: (Int, Int) -> SDL.Rectangle CInt
    tileToSDL tile = 
      let ts, cx, cy :: CInt
          ts = fromIntegral $ (gT $ state^.gameSize)
          cx = (fromIntegral $ fst tile) * ts 
          cy = (fromIntegral $ snd tile) * ts 
      in SDL.Rectangle 
        (P $ V2 cx cy)
        (V2 ts ts)
    colorize :: SDL.Renderer -> ((Int, Int), TileType) -> IO ()
    colorize renderer tile = 
      SDL.rendererDrawColor renderer SDL.$=
        (case (snd tile) of 
          Empty  -> V4 0   0   0   255
          Wall   -> V4 255 255 255 255
          Food   -> V4 0   255 0   255
          Player -> V4 255 255 255 255)
    drawTile :: SDL.Renderer -> ((Int, Int), TileType) -> IO ()
    drawTile renderer tile = do
      colorize renderer tile 
      SDL.fillRect renderer (Just $ tileToSDL (fst tile))
    drawText = drawSDLText window renderer font
    renderGameOverText :: Int -> IO ()
    renderGameOverText score = do
      drawText 
        300 200 (V4 255 0 0 255) 
        (T.pack $ "Game over!")
      drawText
        300 300 (V4 255 0 0 255)
        (T.pack $ "Score: " ++ show score)

main :: IO ()
main = do
  (window, renderer) <- initSDL 640 480
  font <- initSDLFont
  mainLoop
    window 
    renderer 
    font 
    (initialState 640 480 16) 
    =<< (getTime)
  quitSDL window
