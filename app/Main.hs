module Main where

import Controller.Controller
import View.View
import Model.GameState (initialState)

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO window
              black
              10
              initialState
              view
              input
              step
