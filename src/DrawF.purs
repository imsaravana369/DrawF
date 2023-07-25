module DrawF where

import Prelude
import Control.Monad.Free
import Control.Monad.Free (Free, liftF)
import Data.Newtype(class Newtype, wrap)
import Data.Exists (Exists, mkExists)
import DrawF.Type


data DrawF a = ClearCanvas a
             | DrawRectange RectangeProp a
             | DrawSquare SquareProp a
             | DrawCircle CircleProp a
             | GetCursorState (CursorState -> a)
             | MoveTo {x :: Number, y :: Number} a

newtype Draw a = Draw (Free DrawF a)


derive instance newtypeDraw :: Newtype (Draw a) _

_wrapF :: forall a. (Unit -> DrawF Unit) -> Draw Unit
_wrapF fn = wrap <<< liftF $ fn unit

wrapF :: forall a. DrawF a -> Draw a
wrapF = wrap <<< liftF

clearCanvas :: Draw Unit
clearCanvas = _wrapF ClearCanvas

drawRectangle :: RectangeProp -> Draw Unit
drawRectangle = _wrapF <<< DrawRectange 

drawSquare :: SquareProp -> Draw Unit
drawSquare = _wrapF <<< DrawSquare

drawCircle :: CircleProp -> Draw Unit
drawCircle = _wrapF <<< DrawCircle

getCursorState :: Draw CursorState
getCursorState = wrapF $ GetCursorState identity

moveTo :: {x :: Number, y :: Number} -> Draw Unit
moveTo = _wrapF <<< MoveTo