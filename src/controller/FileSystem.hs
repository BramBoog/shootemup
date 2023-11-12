module Controller.FileSystem where

import Model.GameState (GameState, encodeFile, decodeFileStrict)

import System.Directory (getCurrentDirectory)

getGameStateFilePath :: IO String
getGameStateFilePath = do currentDir <- getCurrentDirectory
                          return (currentDir ++ "/GameState.json")

-- Save the GameState in JSON format to "GameState.json in the directory of the game."
saveGameState :: GameState -> IO ()
saveGameState gs = do fp <- getGameStateFilePath
                      encodeFile fp gs

-- Load a GameState from a JSON file on a path relative to the directory of the game.
loadGameState :: IO GameState
loadGameState = do fp <- getGameStateFilePath
                   gsMaybe <- decodeFileStrict fp
                   case gsMaybe of
                     Just gs -> return gs
                     Nothing -> error "Failed to parse the GameState JSON."
