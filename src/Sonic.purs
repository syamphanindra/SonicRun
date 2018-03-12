module Sonic where
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
import Data.Array (snoc, (!!), (..),modifyAt)
import Data.Maybe (Maybe(..), fromMaybe)
import FRP.Behavior (animate)
import FRP.Event.Time (animationFrame)
import PrestoDOM.Core (PrestoDOM)
import PrestoDOM.Elements (imageView, linearLayout, relativeLayout, textView)
import PrestoDOM.Properties (background, backgroundColor, color, gravity, height, id_, imageUrl, margin, orientation, padding, rotation, stroke, text, visibility, width)
import PrestoDOM.Types (Length(..))
import PrestoDOM.Util (render, updateState)
import Ringsmove

foreign import getRand :: Int -> Int -> Int
foreign import getRandWithEff ::forall eff. Int -> Int -> Eff eff Int
drawmissiles :: forall i p . Missile -> PrestoDOM i p
drawmissiles mis =
          imageView
            [
            imageUrl "missileimg"
            ,width (V 100)
            ,height (V 100)
            , margin $ (show  mis.xm) <> "," <> (show mis.ym) <> ",0,0"

            ,visibility mis.visibilitystate

            ]
drawrings :: forall i p.Ring -> PrestoDOM i p
drawrings ring =
        imageView [
         imageUrl "ringimage"
        ,width (V 70)
        ,height (V 70)
        ,margin $ (show  ring.rx) <> "," <> (show ring.ry) <> ",0,0"
        ,visibility ring.ringVisibilty
        ]

drawScoreBoard :: forall i p.State -> PrestoDOM i p
drawScoreBoard state =    linearLayout
        [ id_ "scoreboard"
        , height (V 200)
        , width (V 200)

        , padding "10,10,10,10"
        , orientation "vertical"
        ]
        [ linearLayout
            [ id_  "scorecard"
            , height (V 50)
            , width Match_Parent
            , background "#282B2A"
            , orientation "horizontal"
            , gravity "center"
            , padding "10,0,0,0"
            ]
            [ textView
                [ id_ "score"
                , text $ "Score: " <> (show state.gamescore)
                , height (V 20)
                , width Match_Parent
                , color "white"
                ]
            ]

    --    , renderinstruction state
          ]


world :: forall i p.State -> PrestoDOM i p
world state =
       relativeLayout
           [  width Match_Parent
           ,  height Match_Parent
           ,  id_ "container1"

           ,background "#000000"
           ]
           [ imageView
               [ imageUrl "background1"
                 ,id_ "bg"
                 ,width (V 1000)
                 ,height (V 700)


               ],
               if state.currentscreen == PlayScreen then do
                  relativeLayout
                      [  width Match_Parent
                      ,  height Match_Parent
                      ,  id_ "container2"

                      ,background "ffffff"
                      ][
             imageView
               [
                imageUrl state.sonicstateimage
               , id_ "player"
               , width (V 80)
               , height (V 80)

               ,margin $(show state.x)<> ","<>(show state.y)<>","<>"0,0"
               ]
             ,relativeLayout
                  [
                    width (V 1000)
                    ,height (V 700)
                    ,id_ "missles"
                  ]
                  (map  drawmissiles state.missiles)
                   ,relativeLayout
                        [
                          width (V 1000)
                          ,height (V 700)
                          ,id_ "ringscontainer"
                        ]
                  (map drawrings state.rings)
                  ,drawScoreBoard state

           ]
            else

               imageView
                 [
                  imageUrl "gameoverimage"
                 , id_ "player"
                 , width (V 1000)
                 , height (V 700)
                 ]
                 ,drawScoreBoard state
                 ]



initialState :: State
initialState = { distance : 0
                ,x:100
                ,y:455
                ,speed :0
                ,missiles:[
                {xm:3000,ym:450,imageid:"missile1",visibilitystate:"visible",crash:false}
               ,{xm:1060,ym:455,imageid:"missile2",visibilitystate:"visible",crash:false}
            --    ,{xm:1500,ym:330,imageid:"missile3"}
                ]
                ,currentscreen:PlayScreen
                ,timecount:0
                ,sonicstateimage:"sonicimage"
                ,helpercount:0
                ,timeonground:0
                ,rings:
                [
                 {rx:1200,ry:420,ringVisibilty:"visible"}
                ,{rx:1800,ry:430,ringVisibilty:"visible"}
                ]
                ,gamescore:0
                ,gamespeed:0
                ,rollingcount:0
                ,rollsremain:0
              }

main :: forall eff.Eff(frp :: FRP,dom :: DOM|eff) Unit
main = do
  { stateBeh , updateState} <- render world initialState
  --updateState takes two things a function and a event and return  is discarded by *>
  updateState
    (eval<$> key 32 <*> key 39<*>stateBeh)
    animationFrame *>
  pure unit

eval:: Boolean -> Boolean  -> State -> State
eval  up right state =  case state.currentscreen of
                  PlayScreen -> ((doaction up right) >>> movemissiles>>> collision>>>ringCollision) state
                  GameOverScreen -> state

checkMissileDisplay ::  Missile -> Missile
checkMissileDisplay m = if m.xm>=1000 then do
                            m {visibilitystate="gone"}
                            else do
                              if m.xm == 1000 then do
                                m {visibilitystate="visible",crash=false}
                                else do
                                  if m.crash == true then do
                                    m {visibilitystate="gone"}
                                    else do
                                        m {visibilitystate="visible"}


movemissiles :: State -> State
movemissiles state = do
        let {state,missiles}  = moveEachmissile {state : state , missiles : [] } 0
        let {state,newrings} = moveEachRing {state:state,newrings : []} 0
        state {missiles=missiles,rings=newrings}

moveEachRing :: {state :: State,newrings :: Array Ring} -> Int -> { state :: State , newrings :: Array Ring}
moveEachRing {state,newrings}  x = case state.rings !! x of
                    Nothing -> {state,newrings}
                    Just xi -> moveEachRing {state:state
                                                ,newrings:(newrings `snoc` (newringposition xi state))} (x+1)

newringposition :: Ring -> State -> Ring
newringposition ring state =(resetringposition) ring state


resetringposition :: Ring -> State -> Ring
resetringposition ring state= if ring.rx <=(-5) then
                            ring {rx=1200,ry=(getRand 455 380),ringVisibilty="gone"}
                            else  do
                                if (state.y)/=455 then do
                                  ring
                                  else
                                    if ring.rx>1000 then do
                                      ring {ringVisibilty="gone",rx=ring.rx-2}
                                      else do
                                        if ring.rx==1000 then do
                                          ring {ringVisibilty="visible",rx=ring.rx-2}
                                          else do
                                            ring  {rx=ring.rx-3}

moveEachmissile :: {state :: State,missiles :: Array Missile} -> Int -> { state :: State , missiles :: Array Missile}
moveEachmissile {state,missiles}  x = case state.missiles !! x of
                    Nothing -> {state,missiles}
                    Just xi -> moveEachmissile {state:state
                                                ,missiles:(missiles `snoc` (newposition xi state))} (x+1)

newposition :: Missile ->State -> Missile
newposition m state =(resetmissileposition>>>checkMissileDisplay)  m {xm=m.xm-10-state.gamespeed}

resetmissileposition :: Missile -> Missile
resetmissileposition m = if m.xm <=(-5) then
                            m {xm=1200,ym=(getRand 455 380),crash=false}
                            else
                              m

swapVisibilityofMissile :: Missile -> Missile
swapVisibilityofMissile m = m {visibilitystate ="gone",crash=true}

removeBlastedMissile :: Missile -> State -> Int -> State
removeBlastedMissile m s x = s {missiles =fromMaybe [] $ modifyAt x swapVisibilityofMissile s.missiles}

findCollision ::   Missile -> State -> Int ->State
findCollision  m s x = if m.xm<=s.x+60&&m.xm>=s.x-20&&m.ym<=s.y+40&&m.ym>=s.y-60 && s.sonicstateimage/="rollingsonic" || m.xm<=s.x+60&&m.xm>=s.x-20&&m.ym<=s.y+40&&m.ym>=s.y-60 && s.sonicstateimage/="rollingsonic"&&s.x/=455then do
                            s {currentscreen=GameOverScreen}
                            else do
                              if m.xm<=s.x+60&&m.xm>=s.x-20&&m.ym<=s.y+40&&m.ym>=s.y-60 && s.sonicstateimage=="rollingsonic" then do
                                removeBlastedMissile  m s x
                                else do
                                  if m.xm<=s.x+60 && m.visibilitystate=="gone" then do
                                      removeBlastedMissile  m s x
                                         else do
                                           s



collisionhelper :: State -> Int -> State
collisionhelper state x = case  state.missiles !! x of
                  Nothing -> state
                  Just xi   ->  collisionhelper (findCollision xi state x) (x+1)




collision :: State -> State
collision state =  (collisionhelper state 0)

moveUp ::  State -> State
moveUp  state = state {y=state.y-170,timecount=0,sonicstateimage="sonicjumpimage",helpercount=state.helpercount+1,timeonground=0}

moveDown :: State -> State
moveDown state = state {y=state.y+170,sonicstateimage="sonicimage",helpercount=0,timeonground=0,timecount=0}


doaction :: Boolean -> Boolean  -> State -> State
doaction  true false  state = if state.y==455 && state.timeonground>10 then  moveUp state
                                  else do
                                    if state.y== 285 && state.helpercount>=40 then do
                                      moveDown state
                                        else do
                                          state {helpercount=state.helpercount+1,timeonground=state.timeonground+1}

doaction  false false state = if state.sonicstateimage=="rollingsonic" && state.rollingcount==10 then do
                                      state {sonicstateimage="sonicimage",rollingcount=state.rollingcount+1}
                                        else do
                                            if state.y == 285 && state.timecount==12 then moveDown state
                                              else do
                                                if state.y == 285 && state.timecount>=7 && state.timecount<=9 then do
                                                    state {timecount = state.timecount+1,sonicstateimage="sonicfallingdown",helpercount=0,rollingcount=state.rollingcount+1}
                                                      else do
                                                          state {timecount=state.timecount+1,timeonground=state.timeonground+1,rollingcount=state.rollingcount+1}
doaction  false true state = if state.y == 455 &&state.rollsremain<state.gamescore  then do
                                    state {sonicstateimage="rollingsonic",helpercount=0,rollingcount=0,rollsremain=state.rollsremain+1}
                                      else
                                        if state.y==285 then do
                                          moveDown state {sonicstateimage="sonicimage",helpercount=0}

                                                else
                                                  state {helpercount=0}
doaction  true true state =   state {sonicstateimage="sonicimage",helpercount=0,y=455}

doaction _ _ state = state {sonicstateimage="sonicimage",helpercount=0}
