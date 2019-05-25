module Parser.Tape
  ( Cell
  , Tape
  , value
  , current
  , moveLeft
  , moveRight
  , increment
  , decrement
  , newTape
  ) where

data Cell =
  Cell
    { value :: Int
    }
  deriving (Eq)

instance Show Cell where
  show = show . value

type Cells = [Cell]

data Tape =
  Tape
    { left    :: Cells
    , current :: Cell
    , right   :: Cells
    }
  deriving (Eq)

instance Show Tape where
  show (Tape left current right) =
    (show left) ++ " " ++ (show current) ++ " " ++ (show right)

moveLeft :: Tape -> Tape
moveLeft cell = Tape newLeft newCurrent newRight
  where
    newLeft = (current cell) `append` (left cell)
    (newCurrent, newRight) = top $ right cell

moveRight :: Tape -> Tape
moveRight cell = Tape newLeft newCurrent newRight
  where
    newRight = (current cell) `append` (right cell)
    (newCurrent, newLeft) = top $ left cell

newTape :: Tape
newTape = Tape [] (Cell 0) []

increment :: Tape -> Tape
increment (Tape left current right) = Tape left (add current) right

decrement :: Tape -> Tape
decrement (Tape left current right) = Tape left (substract current) right

top :: Cells -> (Cell, Cells)
top []     = (Cell 0, [])
top (x:xs) = (x, xs)

append :: a -> [a] -> [a]
append = (:)

add :: Cell -> Cell
add (Cell 255) = Cell 0
add (Cell v)   = Cell $ v + 1

substract :: Cell -> Cell
substract (Cell 0) = Cell 255
substract (Cell v) = Cell $ v - 1
