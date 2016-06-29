module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Int as I
import Graphics.Canvas as C
import Control.Monad.Eff.Ref (modifyRef, writeRef, readRef, newRef)
import Data.Array ((..))
import Data.Maybe (Maybe(Just))
import Data.Unit (Unit)
import Math (pi, (%), floor)
import Partial.Unsafe (unsafePartial)

foreign import requestAnimationFrame :: forall e. Eff ( canvas :: C.CANVAS | e ) Unit -> Eff ( canvas :: C.CANVAS | e ) Unit

screenSize :: C.Dimensions
screenSize = { width : 800.0,
               height : 600.0 }

pieceSize :: Number
pieceSize = 56.0

data PieceType = Empty | Mine | Numbered Int
type Piece = {
    pieceType :: PieceType,
    x :: Number,
    y :: Number
}
type Board = Array Piece

type GameState = {
    board :: Board
}
initialGameState :: Number -> Number -> GameState
initialGameState w h =
    let board = (0 .. (I.floor (w * h) - 1)) <#> (\n -> { pieceType : Mine,
                                            x : ((I.toNumber n) % w) * pieceSize,
                                            y : (floor (I.toNumber n / w)) * pieceSize })
    in { board }

update :: GameState -> GameState
update g = g

clearCanvas :: forall e. C.Context2D -> Eff ( canvas :: C.CANVAS | e ) Unit
clearCanvas ctx = void do
    C.setFillStyle "rgb(50, 50, 50)" ctx
    C.fillRect ctx { x : 0.0, y : 0.0, w : screenSize.width, h : screenSize.height }

drawBoard :: forall e. C.Context2D -> Board -> Eff ( canvas :: C.CANVAS | e ) Unit
drawBoard ctx board = void do
    foreachE board (\piece -> void do
        case piece.pieceType of
            Empty -> drawBackground piece
            Mine -> drawBackground piece >>= drawMine
            _ -> pure piece
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

main :: Eff _ Unit
main = void $ unsafePartial $ do
    Just canvas <- C.getCanvasElementById "gamecanvas"
    C.setCanvasDimensions screenSize canvas

    ctx <- C.getContext2D canvas

    stateRef <- newRef (initialGameState 10.0 10.0)

    let loop = void do
            currState <- readRef stateRef
            let nextState = update currState

            clearCanvas ctx
            drawBoard ctx nextState.board

            writeRef stateRef nextState
            requestAnimationFrame loop

    requestAnimationFrame loop
