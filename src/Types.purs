module Types where

import Prelude (class Eq)

type Missile = {
  xm :: Int
 ,ym :: Int
 ,imageid :: String
 ,visibilitystate :: String -- for missiles to display only inside the game screen
 ,crash :: Boolean
}

type Ring = {
  rx :: Int
  ,ry :: Int
  ,ringVisibilty :: String

}

type State = {
  distance :: Int
  ,x :: Int
  ,y :: Int
  ,speed :: Int
  ,missiles :: Array Missile
  ,currentscreen :: GameScreen
  ,timecount :: Int -- used to know when to change the image in jump up position to jump down position
  ,sonicstateimage :: String
  ,helpercount :: Int --helpercount is used to make the sonic come down even after cetain time after jump even though key is hold on pressing
  ,timeonground :: Int
  , rings :: Array Ring
  , gamescore :: Int
  ,gamespeed :: Int
  ,rollingcount :: Int--time to limit rolling
  ,rollsremain:: Int -- number of rolls remaining
}



data GameScreen = PlayScreen | GameOverScreen

instance eqGameScreen :: Eq GameScreen where
  eq PlayScreen PlayScreen = true
  eq GameOverScreen GameOverScreen = true

  eq _ _ = false
