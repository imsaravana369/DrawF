module DrawF where

import Prelude
import Control.Monad.Free
import Control.Monad.Free (Free, liftF)
import Data.Newtype(class Newtype, wrap)
import Data.Exists (Exists, mkExists)

type RectangeProp = { length :: Int, width :: Int}
type SquareProp = {side :: Int}

type CircleProp = {radius :: Number}

type CursorState = {x :: Int, y :: Int}


data DrawF c nextF = SetCanas c nextF
                   | DrawRectange RectangeProp nextF
                   | DrawSquare SquareProp nextF
                   | DrawCircle CircleProp nextF
                   | GetCursorState (CursorState -> nextF)

type DrawWrapper c = DrawF (Exists (DrawF c))
newtype Draw c a = Draw (Free (DrawF c) a)


derive instance newtypeDraw :: Newtype (Draw c a) _

_wrapF :: forall c. (Unit -> DrawF c Unit) -> Draw c Unit
_wrapF fn = wrap <<< liftF $ fn unit

wrapF :: forall c nextF. DrawF c nextF -> Draw c nextF
wrapF = wrap <<< liftF

createCanvas :: forall c.  c -> Draw c Unit
createCanvas = _wrapF <<<  SetCanas

drawRectangle :: forall c.  RectangeProp -> Draw c Unit
drawRectangle = _wrapF <<< DrawRectange 

drawSquare :: forall c.  SquareProp -> Draw c Unit
drawSquare = _wrapF <<< DrawSquare

drawCircle :: forall c.  CircleProp -> Draw c Unit
drawCircle = _wrapF <<< DrawCircle


getCursorState :: forall c. Draw c CursorState
getCursorState = wrapF $ GetCursorState identity
