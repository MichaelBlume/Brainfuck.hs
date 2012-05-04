module Types
( BFState
, JumpTable
, BFProg
, BFRead
) where

import Tape
import Array

type BFState = (Tape, Int)


type JumpTable = [(Int, Int)]
type BFProg = Array Int Char

type BFRead = (BFProg, JumpTable, Int)
