module DrawF.Type where

import Prelude
import Control.Monad.State.Trans(StateT)
import Effect

type RectangeProp = { height :: Number, width :: Number}
type SquareProp = {side :: Number}

type CircleProp = {radius :: Number}

type CursorState = {x :: Number, y :: Number}


type State c = { x :: Number
             , y :: Number
             , color :: String
             , board :: c
             } 


type DrawStateT c a = StateT (State c) Effect a
