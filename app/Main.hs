module Main where

import Controller.Controller
import View.View
import View.Window
import Model.GameState (initialState)

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.IO.Animate (animateIO)

main :: IO ()
main = playIO window
              black
              10
              initialState
              view
              input
              step
