{-# OPTIONS_GHC -w #-}
module Parser where

import Data.Char
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,225) ([37376,32791,4096,0,0,480,32,12064,256,0,0,0,15968,0,3072,0,0,0,1924,128,48272,1024,0,0,0,0,0,0,0,0,0,7680,512,62016,4098,36864,32791,3,256,0,0,0,0,0,0,0,0,0,0,30,2,758,16,2048,0,4096,0,57344,8193,0,15,1,120,8,960,64,7680,512,61440,4096,32768,32775,0,60,4,480,32,0,0,0,48,0,384,0,3072,0,24576,0,0,3,0,24,0,192,0,0,0,0,0,1024,0,24136,512,62016,4098,0,0,36867,188,4,0,0,12068,256,30976,6145,0,0,0,0,0,240,16,6096,128,48320,1024,58496,8197,12288,47,8193,377,8,3017,64,0,1024,0,0,0,0,0,0,0,0,0,12068,256,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Exp","Exp1","AExp","Term","Factor","Pat","let","rec","in","if","then","else","bool","int","var","'()'","'='","'$'","'->'","'+'","'-'","'*'","'/'","'<'","'>'","'<='","'>='","'=='","'('","')'","','","%eof"]
        bit_start = st Prelude.* 35
        bit_end = (st Prelude.+ 1) Prelude.* 35
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..34]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (10) = happyShift action_8
action_0 (13) = happyShift action_9
action_0 (16) = happyShift action_10
action_0 (17) = happyShift action_11
action_0 (18) = happyShift action_12
action_0 (19) = happyShift action_13
action_0 (21) = happyShift action_14
action_0 (32) = happyShift action_15
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (10) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (16) = happyShift action_18
action_2 (17) = happyShift action_19
action_2 (18) = happyShift action_20
action_2 (19) = happyShift action_21
action_2 (32) = happyShift action_22
action_2 (9) = happyGoto action_24
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (13) = happyShift action_9
action_3 (16) = happyShift action_10
action_3 (17) = happyShift action_11
action_3 (18) = happyShift action_12
action_3 (19) = happyShift action_13
action_3 (21) = happyShift action_14
action_3 (32) = happyShift action_15
action_3 (35) = happyAccept
action_3 (5) = happyGoto action_35
action_3 (6) = happyGoto action_5
action_3 (7) = happyGoto action_6
action_3 (8) = happyGoto action_7
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_4

action_5 (23) = happyShift action_28
action_5 (24) = happyShift action_29
action_5 (27) = happyShift action_30
action_5 (28) = happyShift action_31
action_5 (29) = happyShift action_32
action_5 (30) = happyShift action_33
action_5 (31) = happyShift action_34
action_5 _ = happyReduce_7

action_6 (25) = happyShift action_26
action_6 (26) = happyShift action_27
action_6 _ = happyReduce_15

action_7 _ = happyReduce_18

action_8 (11) = happyShift action_25
action_8 (16) = happyShift action_18
action_8 (17) = happyShift action_19
action_8 (18) = happyShift action_20
action_8 (19) = happyShift action_21
action_8 (32) = happyShift action_22
action_8 (9) = happyGoto action_24
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (10) = happyShift action_8
action_9 (13) = happyShift action_9
action_9 (16) = happyShift action_10
action_9 (17) = happyShift action_11
action_9 (18) = happyShift action_12
action_9 (19) = happyShift action_13
action_9 (21) = happyShift action_14
action_9 (32) = happyShift action_15
action_9 (4) = happyGoto action_23
action_9 (5) = happyGoto action_4
action_9 (6) = happyGoto action_5
action_9 (7) = happyGoto action_6
action_9 (8) = happyGoto action_7
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_21

action_11 _ = happyReduce_20

action_12 _ = happyReduce_22

action_13 _ = happyReduce_19

action_14 (16) = happyShift action_18
action_14 (17) = happyShift action_19
action_14 (18) = happyShift action_20
action_14 (19) = happyShift action_21
action_14 (32) = happyShift action_22
action_14 (9) = happyGoto action_17
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (10) = happyShift action_8
action_15 (13) = happyShift action_9
action_15 (16) = happyShift action_10
action_15 (17) = happyShift action_11
action_15 (18) = happyShift action_12
action_15 (19) = happyShift action_13
action_15 (21) = happyShift action_14
action_15 (32) = happyShift action_15
action_15 (4) = happyGoto action_16
action_15 (5) = happyGoto action_4
action_15 (6) = happyGoto action_5
action_15 (7) = happyGoto action_6
action_15 (8) = happyGoto action_7
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (13) = happyShift action_9
action_16 (16) = happyShift action_10
action_16 (17) = happyShift action_11
action_16 (18) = happyShift action_12
action_16 (19) = happyShift action_13
action_16 (21) = happyShift action_14
action_16 (32) = happyShift action_15
action_16 (33) = happyShift action_50
action_16 (34) = happyShift action_51
action_16 (5) = happyGoto action_35
action_16 (6) = happyGoto action_5
action_16 (7) = happyGoto action_6
action_16 (8) = happyGoto action_7
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (22) = happyShift action_49
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_29

action_19 _ = happyReduce_28

action_20 _ = happyReduce_25

action_21 _ = happyReduce_27

action_22 (16) = happyShift action_18
action_22 (17) = happyShift action_19
action_22 (18) = happyShift action_20
action_22 (19) = happyShift action_21
action_22 (32) = happyShift action_22
action_22 (9) = happyGoto action_48
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (13) = happyShift action_9
action_23 (14) = happyShift action_47
action_23 (16) = happyShift action_10
action_23 (17) = happyShift action_11
action_23 (18) = happyShift action_12
action_23 (19) = happyShift action_13
action_23 (21) = happyShift action_14
action_23 (32) = happyShift action_15
action_23 (5) = happyGoto action_35
action_23 (6) = happyGoto action_5
action_23 (7) = happyGoto action_6
action_23 (8) = happyGoto action_7
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (20) = happyShift action_46
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (18) = happyShift action_45
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (16) = happyShift action_10
action_26 (17) = happyShift action_11
action_26 (18) = happyShift action_12
action_26 (19) = happyShift action_13
action_26 (32) = happyShift action_15
action_26 (8) = happyGoto action_44
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (16) = happyShift action_10
action_27 (17) = happyShift action_11
action_27 (18) = happyShift action_12
action_27 (19) = happyShift action_13
action_27 (32) = happyShift action_15
action_27 (8) = happyGoto action_43
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (16) = happyShift action_10
action_28 (17) = happyShift action_11
action_28 (18) = happyShift action_12
action_28 (19) = happyShift action_13
action_28 (32) = happyShift action_15
action_28 (7) = happyGoto action_42
action_28 (8) = happyGoto action_7
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (16) = happyShift action_10
action_29 (17) = happyShift action_11
action_29 (18) = happyShift action_12
action_29 (19) = happyShift action_13
action_29 (32) = happyShift action_15
action_29 (7) = happyGoto action_41
action_29 (8) = happyGoto action_7
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (16) = happyShift action_10
action_30 (17) = happyShift action_11
action_30 (18) = happyShift action_12
action_30 (19) = happyShift action_13
action_30 (32) = happyShift action_15
action_30 (7) = happyGoto action_40
action_30 (8) = happyGoto action_7
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (16) = happyShift action_10
action_31 (17) = happyShift action_11
action_31 (18) = happyShift action_12
action_31 (19) = happyShift action_13
action_31 (32) = happyShift action_15
action_31 (7) = happyGoto action_39
action_31 (8) = happyGoto action_7
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (16) = happyShift action_10
action_32 (17) = happyShift action_11
action_32 (18) = happyShift action_12
action_32 (19) = happyShift action_13
action_32 (32) = happyShift action_15
action_32 (7) = happyGoto action_38
action_32 (8) = happyGoto action_7
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (16) = happyShift action_10
action_33 (17) = happyShift action_11
action_33 (18) = happyShift action_12
action_33 (19) = happyShift action_13
action_33 (32) = happyShift action_15
action_33 (7) = happyGoto action_37
action_33 (8) = happyGoto action_7
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (16) = happyShift action_10
action_34 (17) = happyShift action_11
action_34 (18) = happyShift action_12
action_34 (19) = happyShift action_13
action_34 (32) = happyShift action_15
action_34 (7) = happyGoto action_36
action_34 (8) = happyGoto action_7
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_3

action_36 (25) = happyShift action_26
action_36 (26) = happyShift action_27
action_36 _ = happyReduce_14

action_37 (25) = happyShift action_26
action_37 (26) = happyShift action_27
action_37 _ = happyReduce_13

action_38 (25) = happyShift action_26
action_38 (26) = happyShift action_27
action_38 _ = happyReduce_12

action_39 (25) = happyShift action_26
action_39 (26) = happyShift action_27
action_39 _ = happyReduce_11

action_40 (25) = happyShift action_26
action_40 (26) = happyShift action_27
action_40 _ = happyReduce_10

action_41 (25) = happyShift action_26
action_41 (26) = happyShift action_27
action_41 _ = happyReduce_9

action_42 (25) = happyShift action_26
action_42 (26) = happyShift action_27
action_42 _ = happyReduce_8

action_43 _ = happyReduce_17

action_44 _ = happyReduce_16

action_45 (20) = happyShift action_58
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (10) = happyShift action_8
action_46 (13) = happyShift action_9
action_46 (16) = happyShift action_10
action_46 (17) = happyShift action_11
action_46 (18) = happyShift action_12
action_46 (19) = happyShift action_13
action_46 (21) = happyShift action_14
action_46 (32) = happyShift action_15
action_46 (4) = happyGoto action_57
action_46 (5) = happyGoto action_4
action_46 (6) = happyGoto action_5
action_46 (7) = happyGoto action_6
action_46 (8) = happyGoto action_7
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (10) = happyShift action_8
action_47 (13) = happyShift action_9
action_47 (16) = happyShift action_10
action_47 (17) = happyShift action_11
action_47 (18) = happyShift action_12
action_47 (19) = happyShift action_13
action_47 (21) = happyShift action_14
action_47 (32) = happyShift action_15
action_47 (4) = happyGoto action_56
action_47 (5) = happyGoto action_4
action_47 (6) = happyGoto action_5
action_47 (7) = happyGoto action_6
action_47 (8) = happyGoto action_7
action_47 _ = happyFail (happyExpListPerState 47)

action_48 (33) = happyShift action_54
action_48 (34) = happyShift action_55
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (10) = happyShift action_8
action_49 (13) = happyShift action_9
action_49 (16) = happyShift action_10
action_49 (17) = happyShift action_11
action_49 (18) = happyShift action_12
action_49 (19) = happyShift action_13
action_49 (21) = happyShift action_14
action_49 (32) = happyShift action_15
action_49 (4) = happyGoto action_53
action_49 (5) = happyGoto action_4
action_49 (6) = happyGoto action_5
action_49 (7) = happyGoto action_6
action_49 (8) = happyGoto action_7
action_49 _ = happyFail (happyExpListPerState 49)

action_50 _ = happyReduce_24

action_51 (10) = happyShift action_8
action_51 (13) = happyShift action_9
action_51 (16) = happyShift action_10
action_51 (17) = happyShift action_11
action_51 (18) = happyShift action_12
action_51 (19) = happyShift action_13
action_51 (21) = happyShift action_14
action_51 (32) = happyShift action_15
action_51 (4) = happyGoto action_52
action_51 (5) = happyGoto action_4
action_51 (6) = happyGoto action_5
action_51 (7) = happyGoto action_6
action_51 (8) = happyGoto action_7
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (13) = happyShift action_9
action_52 (16) = happyShift action_10
action_52 (17) = happyShift action_11
action_52 (18) = happyShift action_12
action_52 (19) = happyShift action_13
action_52 (21) = happyShift action_14
action_52 (32) = happyShift action_15
action_52 (33) = happyShift action_63
action_52 (5) = happyGoto action_35
action_52 (6) = happyGoto action_5
action_52 (7) = happyGoto action_6
action_52 (8) = happyGoto action_7
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (13) = happyShift action_9
action_53 (16) = happyShift action_10
action_53 (17) = happyShift action_11
action_53 (18) = happyShift action_12
action_53 (19) = happyShift action_13
action_53 (21) = happyShift action_14
action_53 (32) = happyShift action_15
action_53 (5) = happyGoto action_35
action_53 (6) = happyGoto action_5
action_53 (7) = happyGoto action_6
action_53 (8) = happyGoto action_7
action_53 _ = happyReduce_6

action_54 _ = happyReduce_30

action_55 (16) = happyShift action_18
action_55 (17) = happyShift action_19
action_55 (18) = happyShift action_20
action_55 (19) = happyShift action_21
action_55 (32) = happyShift action_22
action_55 (9) = happyGoto action_62
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (13) = happyShift action_9
action_56 (15) = happyShift action_61
action_56 (16) = happyShift action_10
action_56 (17) = happyShift action_11
action_56 (18) = happyShift action_12
action_56 (19) = happyShift action_13
action_56 (21) = happyShift action_14
action_56 (32) = happyShift action_15
action_56 (5) = happyGoto action_35
action_56 (6) = happyGoto action_5
action_56 (7) = happyGoto action_6
action_56 (8) = happyGoto action_7
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (12) = happyShift action_60
action_57 (13) = happyShift action_9
action_57 (16) = happyShift action_10
action_57 (17) = happyShift action_11
action_57 (18) = happyShift action_12
action_57 (19) = happyShift action_13
action_57 (21) = happyShift action_14
action_57 (32) = happyShift action_15
action_57 (5) = happyGoto action_35
action_57 (6) = happyGoto action_5
action_57 (7) = happyGoto action_6
action_57 (8) = happyGoto action_7
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (10) = happyShift action_8
action_58 (13) = happyShift action_9
action_58 (16) = happyShift action_10
action_58 (17) = happyShift action_11
action_58 (18) = happyShift action_12
action_58 (19) = happyShift action_13
action_58 (21) = happyShift action_14
action_58 (32) = happyShift action_15
action_58 (4) = happyGoto action_59
action_58 (5) = happyGoto action_4
action_58 (6) = happyGoto action_5
action_58 (7) = happyGoto action_6
action_58 (8) = happyGoto action_7
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (12) = happyShift action_67
action_59 (13) = happyShift action_9
action_59 (16) = happyShift action_10
action_59 (17) = happyShift action_11
action_59 (18) = happyShift action_12
action_59 (19) = happyShift action_13
action_59 (21) = happyShift action_14
action_59 (32) = happyShift action_15
action_59 (5) = happyGoto action_35
action_59 (6) = happyGoto action_5
action_59 (7) = happyGoto action_6
action_59 (8) = happyGoto action_7
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (10) = happyShift action_8
action_60 (13) = happyShift action_9
action_60 (16) = happyShift action_10
action_60 (17) = happyShift action_11
action_60 (18) = happyShift action_12
action_60 (19) = happyShift action_13
action_60 (21) = happyShift action_14
action_60 (32) = happyShift action_15
action_60 (4) = happyGoto action_66
action_60 (5) = happyGoto action_4
action_60 (6) = happyGoto action_5
action_60 (7) = happyGoto action_6
action_60 (8) = happyGoto action_7
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (10) = happyShift action_8
action_61 (13) = happyShift action_9
action_61 (16) = happyShift action_10
action_61 (17) = happyShift action_11
action_61 (18) = happyShift action_12
action_61 (19) = happyShift action_13
action_61 (21) = happyShift action_14
action_61 (32) = happyShift action_15
action_61 (4) = happyGoto action_65
action_61 (5) = happyGoto action_4
action_61 (6) = happyGoto action_5
action_61 (7) = happyGoto action_6
action_61 (8) = happyGoto action_7
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (33) = happyShift action_64
action_62 _ = happyFail (happyExpListPerState 62)

action_63 _ = happyReduce_23

action_64 _ = happyReduce_26

action_65 (13) = happyShift action_9
action_65 (16) = happyShift action_10
action_65 (17) = happyShift action_11
action_65 (18) = happyShift action_12
action_65 (19) = happyShift action_13
action_65 (21) = happyShift action_14
action_65 (32) = happyShift action_15
action_65 (5) = happyGoto action_35
action_65 (6) = happyGoto action_5
action_65 (7) = happyGoto action_6
action_65 (8) = happyGoto action_7
action_65 _ = happyReduce_5

action_66 (13) = happyShift action_9
action_66 (16) = happyShift action_10
action_66 (17) = happyShift action_11
action_66 (18) = happyShift action_12
action_66 (19) = happyShift action_13
action_66 (21) = happyShift action_14
action_66 (32) = happyShift action_15
action_66 (5) = happyGoto action_35
action_66 (6) = happyGoto action_5
action_66 (7) = happyGoto action_6
action_66 (8) = happyGoto action_7
action_66 _ = happyReduce_1

action_67 (10) = happyShift action_8
action_67 (13) = happyShift action_9
action_67 (16) = happyShift action_10
action_67 (17) = happyShift action_11
action_67 (18) = happyShift action_12
action_67 (19) = happyShift action_13
action_67 (21) = happyShift action_14
action_67 (32) = happyShift action_15
action_67 (4) = happyGoto action_68
action_67 (5) = happyGoto action_4
action_67 (6) = happyGoto action_5
action_67 (7) = happyGoto action_6
action_67 (8) = happyGoto action_7
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (13) = happyShift action_9
action_68 (16) = happyShift action_10
action_68 (17) = happyShift action_11
action_68 (18) = happyShift action_12
action_68 (19) = happyShift action_13
action_68 (21) = happyShift action_14
action_68 (32) = happyShift action_15
action_68 (5) = happyGoto action_35
action_68 (6) = happyGoto action_5
action_68 (7) = happyGoto action_6
action_68 (8) = happyGoto action_7
action_68 _ = happyReduce_2

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 7 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Rec happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  4 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Application happy_var_1 happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Exp1 happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happyReduce 6 5 happyReduction_5
happyReduction_5 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 5 happyReduction_6
happyReduction_6 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Lambda happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (ArithExp happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (LessThan happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (GreaterThan happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  6 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (LessEqual happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  6 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (GreaterEqual happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  6 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EqualTo happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_1  6 happyReduction_15
happyReduction_15 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Term happy_var_1
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  7 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Times happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  7 happyReduction_17
happyReduction_17 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Div happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  7 happyReduction_18
happyReduction_18 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Factor happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  8 happyReduction_19
happyReduction_19 _
	 =  HappyAbsSyn8
		 (Unit
	)

happyReduce_20 = happySpecReduce_1  8 happyReduction_20
happyReduction_20 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn8
		 (Int happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  8 happyReduction_21
happyReduction_21 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn8
		 (Bool happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  8 happyReduction_22
happyReduction_22 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn8
		 (Var happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happyReduce 5 8 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Pair happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_3  8 happyReduction_24
happyReduction_24 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Nested happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  9 happyReduction_25
happyReduction_25 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn9
		 (PVar happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happyReduce 5 9 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (PPair happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_1  9 happyReduction_27
happyReduction_27 _
	 =  HappyAbsSyn9
		 (PUnit
	)

happyReduce_28 = happySpecReduce_1  9 happyReduction_28
happyReduction_28 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn9
		 (PInt happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  9 happyReduction_29
happyReduction_29 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn9
		 (PBool happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  9 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (PNest happy_var_2
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 35 35 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLet -> cont 10;
	TokenRec -> cont 11;
	TokenIn -> cont 12;
	TokenIf -> cont 13;
	TokenThen -> cont 14;
	TokenElse -> cont 15;
	TokenBool happy_dollar_dollar -> cont 16;
	TokenInt happy_dollar_dollar -> cont 17;
	TokenVar happy_dollar_dollar -> cont 18;
	TokenUnit -> cont 19;
	TokenEq -> cont 20;
	TokenLam -> cont 21;
	TokenArrow -> cont 22;
	TokenPlus -> cont 23;
	TokenMinus -> cont 24;
	TokenTimes -> cont 25;
	TokenDiv -> cont 26;
	TokenLT -> cont 27;
	TokenGT -> cont 28;
	TokenLE -> cont 29;
	TokenGE -> cont 30;
	TokenEqEq -> cont 31;
	TokenOB -> cont 32;
	TokenCB -> cont 33;
	TokenComma -> cont 34;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 35 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Prelude.Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Prelude.Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> HappyIdentity a
happyError' = HappyIdentity Prelude.. (\(tokens, _) -> parseError tokens)
parse tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

type Identifier = String

data Exp
    = Let Pat Exp Exp
    | Rec Identifier Exp Exp
    | Application Exp Exp1
    | Exp1 Exp1
    deriving Show

data Exp1
    = IfThenElse Exp Exp Exp
    | Lambda Pat Exp
    | ArithExp AExp
    deriving Show

data AExp
    = Plus AExp Term
    | Minus AExp Term
    | LessThan AExp Term
    | GreaterThan AExp Term
    | LessEqual AExp Term
    | GreaterEqual AExp Term
    | EqualTo AExp Term
    | Term Term
    deriving Show

data Term
    = Times Term Factor
    | Div Term Factor
    | Factor Factor
    deriving Show

data Factor
    = Unit
    | Int Int
    | Bool Bool
    | Var String
    | Pair Exp Exp
    | Nested Exp
    deriving Show

data Pat
    = PVar Identifier 
    | PPair Pat Pat 
    | PUnit 
    | PInt Int 
    | PBool Bool
    | PNest Pat
    deriving Show

data Token
    = TokenLet
    | TokenRec
    | TokenIn
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenBool Bool
    | TokenInt Int
    | TokenVar String
    | TokenUnit
    | TokenEq
    | TokenLam
    | TokenArrow
    | TokenPlus
    | TokenMinus
    | TokenTimes
    | TokenLT
    | TokenGT
    | TokenLE
    | TokenGE
    | TokenEqEq
    | TokenDiv
    | TokenOB
    | TokenCB
    | TokenComma
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
    | isDigit c = lexNum (c:cs)
lexer ('(':')':cs) = TokenUnit : lexer cs
lexer ('=':'=':cs) = TokenEqEq : lexer cs
lexer ('=':cs) = TokenEq : lexer cs
lexer ('$':cs) = TokenLam : lexer cs
lexer ('-':'>':cs) = TokenArrow : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('/':cs) = TokenDiv : lexer cs
lexer ('<':'=':cs) = TokenLE : lexer cs
lexer ('>':'=':cs) = TokenGE : lexer cs
lexer ('<':cs) = TokenLT : lexer cs
lexer ('>':cs) = TokenGT : lexer cs
lexer ('(':cs) = TokenOB : lexer cs
lexer (')':cs) = TokenCB : lexer cs
lexer (',':cs) = TokenComma : lexer cs

lexNum cs = TokenInt (read num) : lexer rest
    where (num, rest) = span isDigit cs

lexVar cs =
    case span isAlpha cs of
        ("let"  , rest) -> TokenLet         : lexer rest
        ("rec"  , rest) -> TokenRec         : lexer rest
        ("in"   , rest) -> TokenIn          : lexer rest
        ("if"   , rest) -> TokenIf          : lexer rest
        ("then" , rest) -> TokenThen        : lexer rest
        ("else" , rest) -> TokenElse        : lexer rest
        ("True" , rest) -> TokenBool True   : lexer rest
        ("False", rest) -> TokenBool False  : lexer rest
        (var    , rest) -> TokenVar var     : lexer rest
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
