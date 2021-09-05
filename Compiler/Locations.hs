module Compiler.Locations where

import Compiler.Types

envRegister :: Reg
envRegister = RDI

contRegister :: Reg
contRegister = RSI

argRegister :: Reg
argRegister = RDX

frameRegister :: Reg
frameRegister = RSP

heapRegister :: Reg
heapRegister = RBP

graphColourRegisters :: [Reg]
graphColourRegisters = [ RAX
                       , RBX
                       , RCX
                       , RDX
                       , RSI
                       , RDI
                       , R8
                       , R9
                       , R12
                       , R13
                       , R14
                       , R15
                       ]

tempRegister1 :: Reg
tempRegister1 = R10

tempRegister2 :: Reg
tempRegister2 = R11
