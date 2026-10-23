module Game.Types where

type Board = [[Piece]]

type Position = (Int, Int)

type Label = (Int, Int)

data Piece = B | B' | O | O' | X | X' | AO | AO' | AX | AX'
  deriving (Eq, Show)

data Move = PushO | PushX | PushA | Push'
  deriving (Eq, Show)

data Turn = TurnO | TurnX
  deriving (Eq)

instance Show Turn where
  show :: Turn -> String
  show TurnO = "O"
  show TurnX = "X"

data Tree a = Node a [Tree a]
  deriving (Show)
