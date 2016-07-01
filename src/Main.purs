module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Int as I
import Graphics.Canvas as C
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Ref (modifyRef, writeRef, readRef, newRef)
import Data.List (List, (..), (!!), modifyAt)
import Data.Maybe (fromJust, Maybe(Just, Nothing))
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)
import Math (pi, floor)
import Partial.Unsafe (unsafePartial)

foreign import requestAnimationFrame :: forall e. Eff ( canvas :: C.CANVAS | e ) Unit -> Eff ( canvas :: C.CANVAS | e ) Unit
foreign import onMouseUp :: forall e a. Int -> (Number -> Number -> Eff e a) -> Eff e Unit

screenSize :: C.Dimensions
screenSize = { width : 800.0,
               height : 600.0 }

pieceSize :: Number
pieceSize = 32.0

data InputState = NoClick | LeftButton Number Number | RightButton Number Number

data PieceType = Empty | Mine | Numbered Int
instance pieceEq :: Eq PieceType where
    eq Empty Empty = true
    eq Mine Mine = true
    eq (Numbered n) (Numbered m) = n == m
    eq _ _ = false

type Piece =
    { pieceType :: PieceType
    , x :: Number
    , y  :: Number
    }
type Board = List Piece

type GameState =
    { boardWidth :: Int
    , boardHeight :: Int
    , board :: Board
    }
initialGameState :: forall e. Int -> Int -> Eff ( random :: RANDOM | e ) GameState
initialGameState w h = do
    board <- newBoard w h
    pure { board, boardWidth : w, boardHeight : h }

newBoard :: forall e. Int -> Int -> Eff ( random :: RANDOM | e ) Board
newBoard w h = do
    board <- for (Tuple <$> (0 .. (h-1)) <*> (0 .. (w-1))) \(Tuple y x) -> do
        isMine <- random
        let t = if isMine > 0.9 then Mine else Empty
        pure { pieceType : t
             , x : (I.toNumber x) * pieceSize
             , y : (I.toNumber y) * pieceSize
             }
    let board' = numberBoard board
    pure board'
    
    where
        numberBoard :: Board -> Board
        numberBoard board = unsafePartial (
            (Tuple <$> (0 .. (h-1)) <*> (0 .. (w-1))) <#> \(Tuple y x) ->
                let n = x + y * w
                    p = fromJust (board !! n)
                    ns = neighbors x y board
                    
                in if p.pieceType /= Mine then
                    if ns > 0 then
                        p { pieceType = Numbered ns }
                    else
                        p { pieceType = Empty }
                else p       
            )
                
        neighbors :: Int -> Int -> Board -> Int
        neighbors x y board =
            let countMines =   (count (-1) (-1)
                            >>> count (0) (-1)
                            >>> count (1) (-1)
                            >>> count (-1) (0)
                            >>> count (1) (0)
                            >>> count (-1) (1)
                            >>> count (0) (1)
                            >>> count (1) (1))
            in countMines 0
            where
                count :: Int -> Int -> Int -> Int
                count dx dy c =
                    case isMine (x + dx) (y + dy) of
                        Just b -> if b then c + 1 else c
                        Nothing -> c
                
                isMine :: Int -> Int -> Maybe Boolean
                isMine x y
                    | x < 0  || y < 0  = Nothing
                    | x >= w || y >= h = Nothing
                    | otherwise = (board !! (x + y * w)) <#> \p -> p.pieceType == Mine

update :: InputState -> GameState -> Tuple InputState GameState
update input gs =
    case input of
        NoClick -> Tuple input gs
        LeftButton lx ly ->
            let tx = I.floor (lx / pieceSize)
                ty = I.floor (ly / pieceSize)
                n  = tx + ty * gs.boardWidth
                nBoard = unsafePartial $ fromJust $ modifyAt n (\p -> p { pieceType = Mine }) gs.board
            in Tuple NoClick (gs { board = nBoard })
        RightButton _ _ -> Tuple input gs

clearCanvas :: forall e. C.Context2D -> Eff ( canvas :: C.CANVAS | e ) Unit
clearCanvas ctx = void do
    C.setFillStyle "rgb(50, 50, 50)" ctx
    C.fillRect ctx { x : 0.0, y : 0.0, w : screenSize.width, h : screenSize.height }

drawBoard :: forall e. C.Context2D -> Board -> Eff ( canvas :: C.CANVAS | e ) Unit
drawBoard ctx board = void do
    for board (\piece -> void do
        case piece.pieceType of
            Empty -> drawBackground piece
            Mine -> drawBackground piece >>= drawMine
            Numbered _ -> drawBackground piece >>= drawNumbered
    )
    where
        drawBackground :: Piece -> Eff ( canvas :: C.CANVAS | e ) Piece
        drawBackground piece = do
            C.setFillStyle "black" ctx
            C.fillRect ctx { x : piece.x, y : piece.y, w : pieceSize, h : pieceSize }
            C.setFillStyle "#999" ctx
            C.fillRect ctx { x : piece.x + 4.0, y : piece.y + 4.0, w : pieceSize - 4.0, h : pieceSize - 4.0 }
            pure piece

        drawMine :: Piece -> Eff ( canvas :: C.CANVAS | e ) Piece
        drawMine piece = do
            let r = (pieceSize - 8.0) / 2.0
                x = piece.x + r + 6.0
                y = piece.y + r + 6.0
            C.setFillStyle "#b00" ctx
            C.beginPath ctx
            C.arc ctx { x, y, r, start : 0.0, end : 2.0 * pi }
            C.closePath ctx
            C.fill ctx
            let r' = r / 1.5
            C.setFillStyle "#400" ctx
            C.beginPath ctx
            C.arc ctx { x, y, r : r', start : 0.0, end : 2.0 * pi }
            C.closePath ctx
            C.fill ctx
            pure piece

        drawNumbered :: Piece -> Eff ( canvas :: C.CANVAS | e ) Piece
        drawNumbered piece@{ pieceType: (Numbered numOfMines) } = do
            C.setFont ((show (pieceSize * 6.0 / 7.0)) <> "px sans-serif") ctx
            C.setFillStyle "black" ctx
            C.fillText ctx (show numOfMines) (piece.x + pieceSize * 0.25) (piece.y + pieceSize * 0.8)
            pure piece
        drawNumbered piece = pure piece

main :: Eff _ Unit
main = void $ unsafePartial $ do
    Just canvas <- C.getCanvasElementById "gamecanvas"
    C.setCanvasDimensions screenSize canvas

    ctx <- C.getContext2D canvas

    iState <- initialGameState 24 18
    stateRef <- newRef iState
    inputRef <- newRef NoClick
    
    onMouseUp 0 \x y ->
        writeRef inputRef (LeftButton x y)
    onMouseUp 2 \x y ->
        writeRef inputRef (RightButton x y)

    let loop = void do
            input     <- readRef inputRef
            currState <- readRef stateRef
            let nextStates = update input currState
                nextState = snd nextStates
                nextInput = fst nextStates

            clearCanvas ctx
            drawBoard ctx nextState.board

            writeRef inputRef nextInput
            writeRef stateRef nextState
            requestAnimationFrame loop

    requestAnimationFrame loop
  