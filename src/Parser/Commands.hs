module Parser.Commands
  ( execute
  , Command(..)
  , Result
  ) where

import           Control.Lens
import           Control.Lens.Tuple
import           Parser.Tape

data Command
  = MoveLeft
  | MoveRight
  | Incr
  | Decr
  | Write
  | Read
      { input :: Int
      }
  | Loop
      { commands :: [Command]
      }
  deriving (Show, Eq)

type Result = ([Int], [Tape], Tape)

execute :: [Command] -> Result
execute = foldl executeCommand ([], [], newTape)

executeCommand :: Result -> Command -> Result
executeCommand (vs, h, tape) MoveLeft = (vs, tape : h, moveLeft tape)
executeCommand (vs, h, tape) MoveRight = (vs, tape : h, moveRight tape)
executeCommand (vs, h, tape) Incr = (vs, tape : h, increment tape)
executeCommand (vs, h, tape) Decr = (vs, tape : h, decrement tape)
executeCommand (vs, h, tape) Write = ((getCurrent tape) : vs, tape : h, tape)
executeCommand (vs, h, tape) (Read v) = (vs, tape : h, setCurrent tape v)
executeCommand (vs, h, tape) loop@(Loop commands) =
  if (getCurrent $ next ^. _3) == 0
    then next
    else executeCommand next loop
  where
    next = foldl executeCommand (vs, tape : h, tape) commands
