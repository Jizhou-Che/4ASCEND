module Game.Boards where

import Game.Constants
import Game.Types

blank :: Board
blank = replicate rows $ replicate cols B

board :: Board
board = blank
