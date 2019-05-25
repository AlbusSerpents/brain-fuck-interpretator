module Parser.Tape
  ( Cell
  , Tape
  , setCurrent
  , getCurrent
  , moveLeft
  , moveRight
  , increment
  , decrement
  , newTape
  ) where

data Tape =
  Tape
    { left    :: Cells
    , current :: Cell
    , right   :: Cells
    }
  deriving (Eq)

instance Show Tape where
  show (Tape left current right) =
    (show $ reverse left) ++ " " ++ (show current) ++ " " ++ (show right)

data Cell =
  Cell
    { value :: Int
    }
  deriving (Eq)

instance Show Cell where
  show = show . value

type Cells = [Cell]

moveRight :: Tape -> Tape
moveRight cell = Tape newLeft newCurrent newRight
  where
    newLeft = (current cell) : (left cell)
    (newCurrent, newRight) = top $ right cell

moveLeft :: Tape -> Tape
moveLeft cell = Tape newLeft newCurrent newRight
  where
    newRight = (current cell) : (right cell)
    (newCurrent, newLeft) = top $ left cell

newTape :: Tape
newTape = Tape [] (Cell 0) []

increment :: Tape -> Tape
increment (Tape left current right) = Tape left (add current) right

decrement :: Tape -> Tape
decrement (Tape left current right) = Tape left (substract current) right

setCurrent :: Tape -> Int -> Tape
setCurrent (Tape left _ right) value = Tape left (Cell value) right

getCurrent :: Tape -> Int
getCurrent = value . current

add :: Cell -> Cell
add (Cell 255) = Cell 0
add (Cell v)   = Cell $ v + 1

substract :: Cell -> Cell
substract (Cell 0) = Cell 255
substract (Cell v) = Cell $ v - 1

top :: Cells -> (Cell, Cells)
top []     = (Cell 0, [])
top (x:xs) = (x, xs)
