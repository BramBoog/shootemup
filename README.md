# Shoot 'em Up: how to run

## Starting the game

The game can be started from the project directory using the command line.

The follwing command starts the game from a predefined initial state:
```cabal run shootemup```
If you wish to start from a previously created save file (more below), add `"LoadGame"` as a command line argument:
```cabal run shootemup "LoadGame"```

The game will open in a new screen and be ready to play!

## Controls

- Press `s` to shoot.
- Use the up and down arrow keys to move up or down.
- Press `p` to pause the game (you will see everyting stop moving onscreen). Press again to unpause.
- Press `r` to reset the game to the predefined initial state.
- Press `q` to save the game state to a file.

## Playing the game

You as the player are on the left side of the screen. Enemies will spawn on the right side and will move towards the left.
They will shoot in that direction as well, and in some cases also seek you out by directly moving towards you.
In fact, there are five different kinds of enemies, indicated by different colors. They each have a different mix of shooting pattern and player-seeking behaviour.
As time goes on, more and harder varieties of enemies will appear. Each enemy shot will increase your score. Your score and lives are displayed in the top left of the screen.
If a bullet or an enemy hits you, you lose a life. Once all lives are lost, the game is over, at which point you can close the screen or reset the game to start again.

Every once in a while, a powerup will appear above or below you. These will give you a temporary bonus:
either a new weapon, shooting bursts of three bullets straight ahead or in a cone, or increase your speed.

When the game is paused, you can either unpause, reset the game, or create a save file. Pressing the keys to shoot or move has no effect.

### Saving the game

When the game is paused, you can save the game state to disk. This will write the file `"GameState.json"` to the project directory, which contains the full game state
at the time of saving. When you start the game with the `"LoadGame"` argument, the program will parse the game state from the file, and start the game from there.
You will see that everything is the same as when you saved the game! Note that this includes the paused state from when you pressed save, so start the game again by unpausing.
