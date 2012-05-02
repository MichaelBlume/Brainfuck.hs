import Tape

doCommand :: Char -> Tape -> IO Tape
doCommand '<' = return . retreat
doCommand '>' = return . advance
doCommand '+' = return . increment
doCommand '-' = return . decrement
doCommand '.' tape = do
    c <- getChar
    return writeTape tape c
doCommand ',' tape = do
    putChar $ readTape tape
    return tape
doCommand

