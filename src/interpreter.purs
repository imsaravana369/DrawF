module Draw.Interpreter.Web  where

import Prelude
import DrawF.Type
import DrawF
import Graphics.Canvas as C
import Control.Monad.State.Class as ST
import Control.Monad.State.Trans(evalStateT)
import Effect.Class(liftEffect)
import Effect(Effect)
import Data.Number(pi)
import Control.Monad.Free(foldFree)

type WebDrawState a = DrawStateT C.CanvasElement a

interpret :: DrawF ~> WebDrawState
interpret (ClearCanvas next) = do
        canvas <- ST.gets _.board
        ctx <- liftEffect $ C.getContext2D canvas
        {height, width} <- liftEffect $ C.getCanvasDimensions canvas
        liftEffect $ C.clearRect ctx {height, width, x: 0.0, y :0.0}
        pure next

interpret (DrawRectange rectProp next) = do
        canvas <- ST.gets _.board
        ctx <- liftEffect $ C.getContext2D canvas
        st <- ST.get
        liftEffect $ C.rect ctx {height : rectProp.height, width : rectProp.width, x : st.x, y : st.y}
        pure next

interpret (DrawSquare squareProp next) = do
        canvas <- ST.gets _.board
        ctx <- liftEffect $ C.getContext2D canvas
        st <- ST.get
        liftEffect $ C.rect ctx {height : squareProp.side, width : squareProp.side, x : st.x, y : st.y}
        pure next

interpret (DrawCircle circleProp next) = do
        canvas <- ST.gets _.board
        ctx <- liftEffect $ C.getContext2D canvas
        st <- ST.get
        liftEffect $ C.arc ctx  { start : 0.0
                                , end : pi * 2.0
                                , radius : circleProp.radius
                                , useCounterClockwise : false
                                , x : st.x
                                , y : st.y
                                }
        pure next

interpret (GetCursorState nextF) = do
        st <- ST.get
        pure $ nextF { x : st.x , y : st.y}

interpret (MoveTo {x , y} next) = do
        ST.modify_ \st -> st{x = x, y =y}
        pure next


runCanvasDrawing :: forall a. C.CanvasElement -> Draw a -> Effect a
runCanvasDrawing canvasElement (Draw drawCommand) =    
        let 
          initState = {x: 0.0, y : 0.0, color : "#000000", board : canvasElement}
        in         
          evalStateT (foldFree interpret drawCommand) initState