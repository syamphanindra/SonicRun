module Ringsmove where
import Control.Monad.Eff
import Control.Monad.Eff.Console
import DOM
import FRP
import FRP.Behavior.Keyboard
import FRP.Behavior.Time
import Prelude
import Types

import Control.Monad.Eff.Console (log)
import Control.Monad.State (state)
import Data.Array (modifyAt, snoc, (!!), (..))
import Data.Maybe (Maybe(..), fromMaybe)
import FRP.Behavior (animate)
import FRP.Event.Time (animationFrame)
import PrestoDOM.Core (PrestoDOM)
import PrestoDOM.Elements (imageView, linearLayout, relativeLayout, textView)
import PrestoDOM.Properties (background, backgroundColor, color, gravity, height, id_, imageUrl, margin, orientation, padding, rotation, stroke, text, visibility, width)
import PrestoDOM.Types (Length(..))
import PrestoDOM.Util (render, updateState)

ringCollision :: State -> State
ringCollision state = ringCollisionHelper state 0

ringCollisionHelper :: State -> Int -> State
ringCollisionHelper state x =  case  state.rings !! x of
                  Nothing -> state
                  Just xi   ->  ringCollisionHelper (findRingCollision ( xi) state x) (x+1)

findRingCollision :: Ring -> State -> Int -> State
findRingCollision ring s x = if ring.rx<=s.x+60&&ring.rx>=s.x-20&&ring.ry<=s.y+40&&ring.ry>=s.y-60 && ring.ringVisibilty=="visible"  then do
                                if s.gamescore==5 then do
                                    s {gamescore=(s.gamescore+1),rings= changeRingsArray s.rings x,gamespeed=10}
                                        else do
                                            if s.gamescore==10 then do
                                                s {gamescore=(s.gamescore+1),rings= changeRingsArray s.rings x,gamespeed=20}
                                                 else do
                                                        s {gamescore=(s.gamescore+1),rings= changeRingsArray s.rings x}
                                                            else
                                                                s
modifySinglering :: Ring -> Ring
modifySinglering r = r {ringVisibilty="gone"}

changeRingsArray :: Array Ring -> Int -> Array Ring
changeRingsArray r1 x  = fromMaybe [] $ modifyAt x modifySinglering r1
