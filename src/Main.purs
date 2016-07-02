module Main where

import Prelude
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Data.Int as I
import Graphics.Canvas as C
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Ref (modifyRef, writeRef, readRef, newRef)
import Data.HeytingAlgebra ((&&))
import Data.List (List(..), (..), (!!), modifyAt, filter, null, union, difference)
import Data.Maybe (fromJust, Maybe(Just, Nothing))
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)
import Math (pi, floor)
import Partial.Unsafe (unsafePartial)

foreign import requestAnimationFrame :: forall e. Eff ( canvas :: C.CANVAS | e ) Unit -> Eff ( canvas :: C.CANVAS | e ) Unit
foreign import onMouseUp :: forall e a. C.CanvasElement -> Int -> (Number -> Number -> Eff e a) -> Eff e Unit

screenSize :: C.Dimensions
screenSize = { width : 20.0 * pieceSize,
               height : 15.0 * pieceSize }

pieceSize :: Number
pieceSize = 32.0

data Triple a b c = Triple a b c

data Point a = Point a a
instance pointEq :: (Eq a) => Eq (Point a) where
    eq (Point a b) (Point x y) = a == x && b == y
    eq _ _ = false

data InputState = NoClick | LeftButton Number Number | RightButton Number Number

data PieceType = Empty | Mine | Numbered Int
instance pieceEq :: Eq PieceType where
    eq Empty Empty = true
    eq Mine Mine = true
    eq (Numbered _) (Numbered _) = true
    eq _ _ = false

type Piece =
    { pieceType :: PieceType
    , x :: Number
    , y  :: Number
    }
type Board = List Piece

data CoverType = CoverOn | CoverOff
instance coverEq :: Eq CoverType where
    eq CoverOn CoverOn = true
    eq CoverOff CoverOff = true
    eq _ _ = false

type Cover = List
    { coverType :: CoverType
    , x :: Number
    , y :: Number
    }
initialCover :: Int -> Int -> Cover
initialCover w h = (Tuple <$> (0 .. (h-1)) <*> (0 .. (w-1))) <#> \(Tuple y x) ->
    { coverType : CoverOn
    , x : (I.toNumber x) * pieceSize
    , y : (I.toNumber y) * pieceSize
    }

data Animation = NoAnim | Reveal (List (Point Int)) (List (Point Int))
instance animEq :: Eq Animation where
    eq NoAnim NoAnim = true
    eq _ _ = false

type GameState =
    { boardWidth :: Int
    , boardHeight :: Int
    , board :: Board
    , cover :: Cover
    , animation :: Animation
    }
initialGameState :: forall e. Int -> Int -> Eff ( random :: RANDOM | e ) GameState
initialGameState w h = do
    board <- newBoard w h
    pure { board
         , cover : initialCover w h
         , boardWidth : w
         , boardHeight : h
         , animation : NoAnim
         }

--TODO Add a set amount of mines
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
                    

updateAnimation :: GameState -> GameState
updateAnimation gs =
    case gs.animation of
        NoAnim -> gs
        Reveal toCheck checked -> unsafePartial $ go $ findSpan gs toCheck checked
    where
        go :: (Partial) => Triple (List (Point Int)) (List (Point Int)) Int -> GameState
        go (Triple tc@(Cons _ _) ch dl) =
            let nCover = fromJust $ modifyAt dl (\p -> p { coverType = CoverOff }) gs.cover
            in gs { cover = nCover, animation = Reveal tc ch }
        go (Triple Nil _ dl) =
            let nCover = fromJust $ modifyAt dl (\p -> p { coverType = CoverOff }) gs.cover
            in gs { cover = nCover, animation = NoAnim }

findSpan :: GameState -> List (Point Int) -> List (Point Int) -> Triple (List (Point Int)) (List (Point Int)) Int
findSpan gs (Cons p@(Point px py) ps) checked =
    let board = gs.board
        mp = pieceAt board px py
    in case mp of
        Just x   -> if x.pieceType == Empty then
                        Triple (union (difference (mPieces p) checked) ps) (Cons p checked) (px + py * gs.boardWidth)
                    else if x.pieceType == Numbered 0 then
                        Triple ps (Cons p checked) (px + py * gs.boardWidth)
                    else
                        findSpan gs ps (Cons p checked)
        Nothing  -> findSpan gs ps checked
    where
        mPieces :: Point Int -> List (Point Int)
        mPieces (Point x y) =
            (Cons (Point (x - 1) y)
            (Cons (Point (x + 1) y)
            (Cons (Point x (y - 1))
            (Cons (Point x (y + 1))
            (Cons (Point (x - 1) (y - 1))
            (Cons (Point (x + 1) (y - 1))
            (Cons (Point (x - 1) (y + 1))
            (Cons (Point (x + 1) (y + 1))
            Nil))))))))
        
        pieceAt :: Board -> Int -> Int -> Maybe Piece
        pieceAt board x y
            | x < 0 || y < 0                             = Nothing
            | x >= gs.boardWidth || y >= gs.boardHeight  = Nothing
            | otherwise = board !! (x + y * gs.boardWidth)
    
findSpan gs Nil ch@(Cons (Point sx sy) _) = Triple Nil ch (sx + sy * gs.boardWidth)
findSpan _ Nil Nil = Triple Nil Nil 0

update :: InputState -> GameState -> Tuple InputState GameState
update input gs =
    let ngs = updateAnimation gs
    in case input of
        NoClick -> Tuple input ngs
        LeftButton lx ly ->
            if ngs.animation == NoAnim then
                let tx = I.floor (lx / pieceSize)
                    ty = I.floor (ly / pieceSize)
                in case ngs.cover !! (tx + ty * ngs.boardWidth) of
                    Just p   -> if p.coverType == CoverOn then
                                    Tuple NoClick (ngs { animation = Reveal (Cons (Point tx ty) Nil) Nil })
                                else
                                    Tuple NoClick ngs
                    Nothing -> Tuple NoClick ngs
            else
                Tuple NoClick ngs
        RightButton _ _ -> Tuple input ngs

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
        
drawCover :: forall e. C.Context2D -> Cover -> Eff ( canvas :: C.CANVAS | e ) Unit
drawCover ctx cover = void do
    for cover \c -> do
        ifM (pure (c.coverType == CoverOff))
            (pure unit)
            (void do
                C.setFillStyle "black" ctx
                C.fillRect ctx { x : c.x, y : c.y, w : pieceSize, h : pieceSize }
                C.setFillStyle "#333" ctx
                C.fillRect ctx { x : c.x + 4.0, y : c.y + 4.0, w : pieceSize - 4.0, h : pieceSize - 4.0 })

main :: Eff _ Unit
main = void $ unsafePartial $ do
    Just canvas <- C.getCanvasElementById "gamecanvas"
    C.setCanvasDimensions screenSize canvas

    ctx <- C.getContext2D canvas

    iState <- initialGameState 20 15
    stateRef <- newRef iState
    inputRef <- newRef NoClick
    
    onMouseUp canvas 0 \x y ->
        writeRef inputRef (LeftButton x y)
    onMouseUp canvas 2 \x y ->
        writeRef inputRef (RightButton x y)

    let loop = void do
            input     <- readRef inputRef
            currState <- readRef stateRef
            let nextStates = update input currState
                nextState = snd nextStates
                nextInput = fst nextStates

            --clearCanvas ctx
            drawBoard ctx nextState.board
            drawCover ctx nextState.cover

            writeRef inputRef nextInput
            writeRef stateRef nextState
            requestAnimationFrame loop

    requestAnimationFrame loop
  