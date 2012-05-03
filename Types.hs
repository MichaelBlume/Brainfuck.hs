module Types
( BFState
, JumpTable
, BFProg
, BFRead
) where

import Tape

type BFState = (Tape, Int)


type JumpTable = [(Int, Int)]
type BFProg = String

type BFRead = (BFProg, JumpTable, Int)
