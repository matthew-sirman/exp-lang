module X86Printer (
    printX86
) where

import X86

printX86 code = printTextSection ++ printRegions rs
    where
        printTextSection =
            "section .text\n" 
            ++ concat (map (\r -> "    global " ++ label r ++ "\n") gs)
            ++ "\n"

        printRegions [] = ""
        printRegions (r:rs) = printR r ++ printRegions rs

        printR (X86Region name is) = 
            name ++ ":\n"
            ++ concat (map (\i -> "    " ++ printInstruction i ++ "\n") is)

        printInstruction (X86Add rd ra) = "add " ++ printReg rd ++ ", " ++ printReg ra
        printInstruction (X86Sub rd ra) = "sub " ++ printReg rd ++ ", " ++ printReg ra
        printInstruction (X86Push r) = "push " ++ printReg r
        printInstruction (X86Pop r) = "pop " ++ printReg r
        printInstruction (X86Mov r i) = "mov " ++ printReg r ++ ", " ++ show i
        printInstruction (X86Ret) = "ret"

        printReg RAX = "rax"
        printReg RBX = "rbx"
        printReg RCX = "rcx"
        printReg RDX = "rdx"

        gs = globals $ textSection code
        rs = regions code
