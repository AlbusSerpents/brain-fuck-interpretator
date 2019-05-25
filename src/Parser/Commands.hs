module Parser.Commands
  ( execute
  , Command(..)
  , Result
  ) where

import           Control.Lens
import           Control.Monad.Trans.Writer
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

type Result = ([Int], Tape)

type Parser = Writer [Tape] Result

execute :: [Command] -> (Result, [Tape])
execute = completeParse . foldl parseCommand newParser
  where
    completeParse = over _2 reverse . applyLast . runWriter
    newParser = return ([], newTape)
    applyLast (result, history) = (result, (result ^. _2) : history)

parseCommand :: Parser -> Command -> Parser
parseCommand resultWriter command =
  mapWriter
    (\(result, w) -> (executeCommand result command, (result ^. _2) : w))
    resultWriter

executeCommand :: Result -> Command -> Result
executeCommand (vs, tape) MoveLeft = (vs, moveLeft tape)
executeCommand (vs, tape) MoveRight = (vs, moveRight tape)
executeCommand (vs, tape) Incr = (vs, increment tape)
executeCommand (vs, tape) Decr = (vs, decrement tape)
executeCommand (vs, tape) Write = ((getCurrent tape) : vs, tape)
executeCommand (vs, tape) (Read v) = (vs, setCurrent tape v)
executeCommand (vs, tape) loop@(Loop commands) =
  if (getCurrent $ next ^. _2) == 0
    then next
    else executeCommand next loop
  where
    next = foldl executeCommand (vs, tape) commands
