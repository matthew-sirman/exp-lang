module Asm.X86_64Printer where

import Generators.X86_64

instance Show RegAccess where
    show (RegAccess Q8 R8) = "%r8"
    show (RegAccess E4 R8) = "%r8d"
    show (RegAccess W2 R8) = "%r8h"
    show (RegAccess B1 R8) = "%r8b"
    
    show (RegAccess Q8 R9) = "%r9"
    show (RegAccess E4 R9) = "%r9d"
    show (RegAccess W2 R9) = "%r9h"
    show (RegAccess B1 R9) = "%r9b"
    
    show (RegAccess Q8 R10) = "%r10"
    show (RegAccess E4 R10) = "%r10d"
    show (RegAccess W2 R10) = "%r10h"
    show (RegAccess B1 R10) = "%r10b"
    
    show (RegAccess Q8 R11) = "%r11"
    show (RegAccess E4 R11) = "%r11d"
    show (RegAccess W2 R11) = "%r11h"
    show (RegAccess B1 R11) = "%r11b"
    
    show (RegAccess Q8 R12) = "%r12"
    show (RegAccess E4 R12) = "%r12d"
    show (RegAccess W2 R12) = "%r12h"
    show (RegAccess B1 R12) = "%r12b"
    
    show (RegAccess Q8 R13) = "%r13"
    show (RegAccess E4 R13) = "%r13d"
    show (RegAccess W2 R13) = "%r13h"
    show (RegAccess B1 R13) = "%r13b"
    
    show (RegAccess Q8 R14) = "%r14"
    show (RegAccess E4 R14) = "%r14d"
    show (RegAccess W2 R14) = "%r14h"
    show (RegAccess B1 R14) = "%r14b"
    
    show (RegAccess Q8 R15) = "%r15"
    show (RegAccess E4 R15) = "%r15d"
    show (RegAccess W2 R15) = "%r15h"
    show (RegAccess B1 R15) = "%r15b"
    
    show (RegAccess Q8 RAX) = "%rax"
    show (RegAccess E4 RAX) = "%eax"
    show (RegAccess W2 RAX) = "%ax"
    show (RegAccess B1 RAX) = "%al"

    show (RegAccess Q8 RBX) = "%rbx"
    show (RegAccess E4 RBX) = "%ebx"
    show (RegAccess W2 RBX) = "%bx"
    show (RegAccess B1 RBX) = "%bl"

    show (RegAccess Q8 RCX) = "%rcx"
    show (RegAccess E4 RCX) = "%ecx"
    show (RegAccess W2 RCX) = "%cx"
    show (RegAccess B1 RCX) = "%cl"

    show (RegAccess Q8 RDX) = "%rdx"
    show (RegAccess E4 RDX) = "%edx"
    show (RegAccess W2 RDX) = "%dx"
    show (RegAccess B1 RDX) = "%dl"

    show (RegAccess Q8 RDI) = "%rdi"
    show (RegAccess E4 RDI) = "%edi"
    show (RegAccess W2 RDI) = "%di"
    show (RegAccess B1 RDI) = "%dil"

    show (RegAccess Q8 RSI) = "%rsi"
    show (RegAccess E4 RSI) = "%esi"
    show (RegAccess W2 RSI) = "%si"
    show (RegAccess B1 RSI) = "%sil"

instance Show Immediate where
    show (I64 i) = "$" ++ show i

instance Show Value where
    show (Immediate i) = show i
    show (Register r) = show r

instance Show Instruction where
    show = show'
        where
            show' (Add v r)     = "add" ++ [suffix r] ++ "    " ++ show v ++ ", " ++ show r
            show' (Sub v r)     = "sub" ++ [suffix r] ++ "    " ++ show v ++ ", " ++ show r
            show' (Mul v)       = "imul" ++ [suffix v] ++ "   " ++ show v
            show' (CQTO)        = "cqto"
            show' (Div v)       = "idiv" ++ [suffix v] ++ "   " ++ show v
            show' (Cmp l r)     = "cmp" ++ [suffix r] ++ "    " ++ show l ++ ", " ++ show r
            show' (SetEQ r)     = "sete    " ++ show r
            show' (SetLT r)     = "setl    " ++ show r
            show' (SetGT r)     = "setg    " ++ show r
            show' (SetLE r)     = "setle   " ++ show r
            show' (SetGE r)     = "setge   " ++ show r
            show' (Mov v r)     = "mov"  ++ [suffix r] ++ "    " ++ show v ++ ", " ++ show r
            show' (LEA l r)     = "lea" ++ [suffix r] ++ "    " ++ l ++ "(%rip), " ++ show r
            show' (Write v r)   = "mov" ++ [suffix r] ++ "    " ++ show v ++ ", (" ++ show r ++ ")"
            show' (Read r v)    = "mov" ++ [suffix v] ++ "    (" ++ show r ++ "), " ++ show v
            show' (DCall lab)   = "call    " ++ lab
            show' (ICall r)     = "call    *" ++ show r
            show' (JmpNE lab)   = "jne     " ++ lab
            show' (Jmp lab)     = "jmp     " ++ lab
            show' (Ret)         = "ret\n"
            show' (Push r)      = "push" ++ [vSuffix r] ++ "   " ++ show r
            show' (Pop r)       = "pop" ++ [suffix r] ++ "    " ++ show r

            suffix (RegAccess Q8 _) = 'q'
            suffix (RegAccess E4 _) = 'd'
            suffix (RegAccess W2 _) = 'w'
            suffix (RegAccess B1 _) = 'b'

            vSuffix (Immediate (I64 _)) = 'q'
            vSuffix (Register r) = suffix r

instance Show X86_64Code where
    show (X86_64Code t rs) = printText t ('\n' : foldr printRegion "" rs)
        where
            printRegion :: Region -> String -> String
            printRegion (Region lab is) acc =
                lab ++ ":\n" ++
                foldr (\i -> ((addTab (show i) ++ "\n") ++)) acc is
            
            printText :: TextSection -> String -> String
            printText (TextSection gs es) acc =
                ".text\n" ++
                concatMap mkGlobal gs ++
                concatMap mkExtern es ++
                acc
    
            tab, global, extern :: String
            tab = "    "
            global = ".global "
            extern = ".extern "
    
            addTab, mkGlobal, mkExtern :: String -> String
            addTab = (tab ++)
            mkGlobal s = (tab ++ global ++ s ++ "\n")
            mkExtern s = (tab ++ extern ++ s ++ "\n")

