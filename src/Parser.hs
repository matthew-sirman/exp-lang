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
happyExpList = Happy_Data_Array.listArray (0,194) ([51712,16395,2048,0,0,60,4,754,16,0,0,32768,249,0,24,0,0,51712,16395,0,0,0,0,0,0,0,0,0,3840,256,48288,1024,61952,28674,0,16,0,0,0,0,0,0,0,0,0,3840,256,48512,1024,61440,4096,49152,16387,0,15,1,60,4,240,16,960,64,3840,256,15360,1024,61440,4096,0,0,0,16,40960,188,4,24576,0,32768,1,0,6,0,24,0,96,0,384,0,1536,0,0,0,0,0,3018,64,0,1536,48288,1024,0,0,51712,16395,8192,47,3,0,0,0,0,960,64,12192,256,48320,1024,62080,4098,51712,16395,0,0,2,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Exp","Exp1","AExp","Term","Factor","Pat","let","in","if","then","else","bool","int","var","'()'","'='","'$'","'->'","'+'","'-'","'*'","'/'","'<'","'>'","'<='","'>='","'=='","'('","')'","','","%eof"]
        bit_start = st Prelude.* 34
        bit_end = (st Prelude.+ 1) Prelude.* 34
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..33]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (10) = happyShift action_2
action_0 (12) = happyShift action_8
action_0 (15) = happyShift action_9
action_0 (16) = happyShift action_10
action_0 (17) = happyShift action_11
action_0 (18) = happyShift action_12
action_0 (20) = happyShift action_13
action_0 (31) = happyShift action_14
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (10) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (15) = happyShift action_17
action_2 (16) = happyShift action_18
action_2 (17) = happyShift action_19
action_2 (18) = happyShift action_20
action_2 (31) = happyShift action_21
action_2 (9) = happyGoto action_33
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (12) = happyShift action_8
action_3 (15) = happyShift action_9
action_3 (16) = happyShift action_10
action_3 (17) = happyShift action_11
action_3 (18) = happyShift action_12
action_3 (20) = happyShift action_13
action_3 (31) = happyShift action_14
action_3 (34) = happyAccept
action_3 (5) = happyGoto action_32
action_3 (6) = happyGoto action_5
action_3 (7) = happyGoto action_6
action_3 (8) = happyGoto action_7
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (22) = happyShift action_25
action_5 (23) = happyShift action_26
action_5 (26) = happyShift action_27
action_5 (27) = happyShift action_28
action_5 (28) = happyShift action_29
action_5 (29) = happyShift action_30
action_5 (30) = happyShift action_31
action_5 _ = happyReduce_6

action_6 (24) = happyShift action_23
action_6 (25) = happyShift action_24
action_6 _ = happyReduce_14

action_7 _ = happyReduce_17

action_8 (10) = happyShift action_2
action_8 (12) = happyShift action_8
action_8 (15) = happyShift action_9
action_8 (16) = happyShift action_10
action_8 (17) = happyShift action_11
action_8 (18) = happyShift action_12
action_8 (20) = happyShift action_13
action_8 (31) = happyShift action_14
action_8 (4) = happyGoto action_22
action_8 (5) = happyGoto action_4
action_8 (6) = happyGoto action_5
action_8 (7) = happyGoto action_6
action_8 (8) = happyGoto action_7
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_20

action_10 _ = happyReduce_19

action_11 _ = happyReduce_21

action_12 _ = happyReduce_18

action_13 (15) = happyShift action_17
action_13 (16) = happyShift action_18
action_13 (17) = happyShift action_19
action_13 (18) = happyShift action_20
action_13 (31) = happyShift action_21
action_13 (9) = happyGoto action_16
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (10) = happyShift action_2
action_14 (12) = happyShift action_8
action_14 (15) = happyShift action_9
action_14 (16) = happyShift action_10
action_14 (17) = happyShift action_11
action_14 (18) = happyShift action_12
action_14 (20) = happyShift action_13
action_14 (31) = happyShift action_14
action_14 (4) = happyGoto action_15
action_14 (5) = happyGoto action_4
action_14 (6) = happyGoto action_5
action_14 (7) = happyGoto action_6
action_14 (8) = happyGoto action_7
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (12) = happyShift action_8
action_15 (15) = happyShift action_9
action_15 (16) = happyShift action_10
action_15 (17) = happyShift action_11
action_15 (18) = happyShift action_12
action_15 (20) = happyShift action_13
action_15 (31) = happyShift action_14
action_15 (32) = happyShift action_47
action_15 (33) = happyShift action_48
action_15 (5) = happyGoto action_32
action_15 (6) = happyGoto action_5
action_15 (7) = happyGoto action_6
action_15 (8) = happyGoto action_7
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (21) = happyShift action_46
action_16 _ = happyFail (happyExpListPerState 16)

action_17 _ = happyReduce_28

action_18 _ = happyReduce_27

action_19 _ = happyReduce_24

action_20 _ = happyReduce_26

action_21 (15) = happyShift action_17
action_21 (16) = happyShift action_18
action_21 (17) = happyShift action_19
action_21 (18) = happyShift action_20
action_21 (31) = happyShift action_21
action_21 (9) = happyGoto action_45
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (12) = happyShift action_8
action_22 (13) = happyShift action_44
action_22 (15) = happyShift action_9
action_22 (16) = happyShift action_10
action_22 (17) = happyShift action_11
action_22 (18) = happyShift action_12
action_22 (20) = happyShift action_13
action_22 (31) = happyShift action_14
action_22 (5) = happyGoto action_32
action_22 (6) = happyGoto action_5
action_22 (7) = happyGoto action_6
action_22 (8) = happyGoto action_7
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (15) = happyShift action_9
action_23 (16) = happyShift action_10
action_23 (17) = happyShift action_11
action_23 (18) = happyShift action_12
action_23 (31) = happyShift action_14
action_23 (8) = happyGoto action_43
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (15) = happyShift action_9
action_24 (16) = happyShift action_10
action_24 (17) = happyShift action_11
action_24 (18) = happyShift action_12
action_24 (31) = happyShift action_14
action_24 (8) = happyGoto action_42
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (15) = happyShift action_9
action_25 (16) = happyShift action_10
action_25 (17) = happyShift action_11
action_25 (18) = happyShift action_12
action_25 (31) = happyShift action_14
action_25 (7) = happyGoto action_41
action_25 (8) = happyGoto action_7
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (15) = happyShift action_9
action_26 (16) = happyShift action_10
action_26 (17) = happyShift action_11
action_26 (18) = happyShift action_12
action_26 (31) = happyShift action_14
action_26 (7) = happyGoto action_40
action_26 (8) = happyGoto action_7
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (15) = happyShift action_9
action_27 (16) = happyShift action_10
action_27 (17) = happyShift action_11
action_27 (18) = happyShift action_12
action_27 (31) = happyShift action_14
action_27 (7) = happyGoto action_39
action_27 (8) = happyGoto action_7
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (15) = happyShift action_9
action_28 (16) = happyShift action_10
action_28 (17) = happyShift action_11
action_28 (18) = happyShift action_12
action_28 (31) = happyShift action_14
action_28 (7) = happyGoto action_38
action_28 (8) = happyGoto action_7
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (15) = happyShift action_9
action_29 (16) = happyShift action_10
action_29 (17) = happyShift action_11
action_29 (18) = happyShift action_12
action_29 (31) = happyShift action_14
action_29 (7) = happyGoto action_37
action_29 (8) = happyGoto action_7
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (15) = happyShift action_9
action_30 (16) = happyShift action_10
action_30 (17) = happyShift action_11
action_30 (18) = happyShift action_12
action_30 (31) = happyShift action_14
action_30 (7) = happyGoto action_36
action_30 (8) = happyGoto action_7
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (15) = happyShift action_9
action_31 (16) = happyShift action_10
action_31 (17) = happyShift action_11
action_31 (18) = happyShift action_12
action_31 (31) = happyShift action_14
action_31 (7) = happyGoto action_35
action_31 (8) = happyGoto action_7
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_2

action_33 (19) = happyShift action_34
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (10) = happyShift action_2
action_34 (12) = happyShift action_8
action_34 (15) = happyShift action_9
action_34 (16) = happyShift action_10
action_34 (17) = happyShift action_11
action_34 (18) = happyShift action_12
action_34 (20) = happyShift action_13
action_34 (31) = happyShift action_14
action_34 (4) = happyGoto action_54
action_34 (5) = happyGoto action_4
action_34 (6) = happyGoto action_5
action_34 (7) = happyGoto action_6
action_34 (8) = happyGoto action_7
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (24) = happyShift action_23
action_35 (25) = happyShift action_24
action_35 _ = happyReduce_13

action_36 (24) = happyShift action_23
action_36 (25) = happyShift action_24
action_36 _ = happyReduce_12

action_37 (24) = happyShift action_23
action_37 (25) = happyShift action_24
action_37 _ = happyReduce_11

action_38 (24) = happyShift action_23
action_38 (25) = happyShift action_24
action_38 _ = happyReduce_10

action_39 (24) = happyShift action_23
action_39 (25) = happyShift action_24
action_39 _ = happyReduce_9

action_40 (24) = happyShift action_23
action_40 (25) = happyShift action_24
action_40 _ = happyReduce_8

action_41 (24) = happyShift action_23
action_41 (25) = happyShift action_24
action_41 _ = happyReduce_7

action_42 _ = happyReduce_16

action_43 _ = happyReduce_15

action_44 (10) = happyShift action_2
action_44 (12) = happyShift action_8
action_44 (15) = happyShift action_9
action_44 (16) = happyShift action_10
action_44 (17) = happyShift action_11
action_44 (18) = happyShift action_12
action_44 (20) = happyShift action_13
action_44 (31) = happyShift action_14
action_44 (4) = happyGoto action_53
action_44 (5) = happyGoto action_4
action_44 (6) = happyGoto action_5
action_44 (7) = happyGoto action_6
action_44 (8) = happyGoto action_7
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (32) = happyShift action_51
action_45 (33) = happyShift action_52
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (10) = happyShift action_2
action_46 (12) = happyShift action_8
action_46 (15) = happyShift action_9
action_46 (16) = happyShift action_10
action_46 (17) = happyShift action_11
action_46 (18) = happyShift action_12
action_46 (20) = happyShift action_13
action_46 (31) = happyShift action_14
action_46 (4) = happyGoto action_50
action_46 (5) = happyGoto action_4
action_46 (6) = happyGoto action_5
action_46 (7) = happyGoto action_6
action_46 (8) = happyGoto action_7
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_23

action_48 (10) = happyShift action_2
action_48 (12) = happyShift action_8
action_48 (15) = happyShift action_9
action_48 (16) = happyShift action_10
action_48 (17) = happyShift action_11
action_48 (18) = happyShift action_12
action_48 (20) = happyShift action_13
action_48 (31) = happyShift action_14
action_48 (4) = happyGoto action_49
action_48 (5) = happyGoto action_4
action_48 (6) = happyGoto action_5
action_48 (7) = happyGoto action_6
action_48 (8) = happyGoto action_7
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (12) = happyShift action_8
action_49 (15) = happyShift action_9
action_49 (16) = happyShift action_10
action_49 (17) = happyShift action_11
action_49 (18) = happyShift action_12
action_49 (20) = happyShift action_13
action_49 (31) = happyShift action_14
action_49 (32) = happyShift action_58
action_49 (5) = happyGoto action_32
action_49 (6) = happyGoto action_5
action_49 (7) = happyGoto action_6
action_49 (8) = happyGoto action_7
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (12) = happyShift action_8
action_50 (15) = happyShift action_9
action_50 (16) = happyShift action_10
action_50 (17) = happyShift action_11
action_50 (18) = happyShift action_12
action_50 (20) = happyShift action_13
action_50 (31) = happyShift action_14
action_50 (5) = happyGoto action_32
action_50 (6) = happyGoto action_5
action_50 (7) = happyGoto action_6
action_50 (8) = happyGoto action_7
action_50 _ = happyReduce_5

action_51 _ = happyReduce_29

action_52 (15) = happyShift action_17
action_52 (16) = happyShift action_18
action_52 (17) = happyShift action_19
action_52 (18) = happyShift action_20
action_52 (31) = happyShift action_21
action_52 (9) = happyGoto action_57
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (12) = happyShift action_8
action_53 (14) = happyShift action_56
action_53 (15) = happyShift action_9
action_53 (16) = happyShift action_10
action_53 (17) = happyShift action_11
action_53 (18) = happyShift action_12
action_53 (20) = happyShift action_13
action_53 (31) = happyShift action_14
action_53 (5) = happyGoto action_32
action_53 (6) = happyGoto action_5
action_53 (7) = happyGoto action_6
action_53 (8) = happyGoto action_7
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (11) = happyShift action_55
action_54 (12) = happyShift action_8
action_54 (15) = happyShift action_9
action_54 (16) = happyShift action_10
action_54 (17) = happyShift action_11
action_54 (18) = happyShift action_12
action_54 (20) = happyShift action_13
action_54 (31) = happyShift action_14
action_54 (5) = happyGoto action_32
action_54 (6) = happyGoto action_5
action_54 (7) = happyGoto action_6
action_54 (8) = happyGoto action_7
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (10) = happyShift action_2
action_55 (12) = happyShift action_8
action_55 (15) = happyShift action_9
action_55 (16) = happyShift action_10
action_55 (17) = happyShift action_11
action_55 (18) = happyShift action_12
action_55 (20) = happyShift action_13
action_55 (31) = happyShift action_14
action_55 (4) = happyGoto action_61
action_55 (5) = happyGoto action_4
action_55 (6) = happyGoto action_5
action_55 (7) = happyGoto action_6
action_55 (8) = happyGoto action_7
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (10) = happyShift action_2
action_56 (12) = happyShift action_8
action_56 (15) = happyShift action_9
action_56 (16) = happyShift action_10
action_56 (17) = happyShift action_11
action_56 (18) = happyShift action_12
action_56 (20) = happyShift action_13
action_56 (31) = happyShift action_14
action_56 (4) = happyGoto action_60
action_56 (5) = happyGoto action_4
action_56 (6) = happyGoto action_5
action_56 (7) = happyGoto action_6
action_56 (8) = happyGoto action_7
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (32) = happyShift action_59
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_22

action_59 _ = happyReduce_25

action_60 (12) = happyShift action_8
action_60 (15) = happyShift action_9
action_60 (16) = happyShift action_10
action_60 (17) = happyShift action_11
action_60 (18) = happyShift action_12
action_60 (20) = happyShift action_13
action_60 (31) = happyShift action_14
action_60 (5) = happyGoto action_32
action_60 (6) = happyGoto action_5
action_60 (7) = happyGoto action_6
action_60 (8) = happyGoto action_7
action_60 _ = happyReduce_4

action_61 (12) = happyShift action_8
action_61 (15) = happyShift action_9
action_61 (16) = happyShift action_10
action_61 (17) = happyShift action_11
action_61 (18) = happyShift action_12
action_61 (20) = happyShift action_13
action_61 (31) = happyShift action_14
action_61 (5) = happyGoto action_32
action_61 (6) = happyGoto action_5
action_61 (7) = happyGoto action_6
action_61 (8) = happyGoto action_7
action_61 _ = happyReduce_1

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

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Application happy_var_1 happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Exp1 happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happyReduce 6 5 happyReduction_4
happyReduction_4 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (IfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 4 5 happyReduction_5
happyReduction_5 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (Lambda happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (ArithExp happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  6 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (LessThan happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  6 happyReduction_10
happyReduction_10 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (GreaterThan happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  6 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (LessEqual happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  6 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (GreaterEqual happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  6 happyReduction_13
happyReduction_13 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (EqualTo happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  6 happyReduction_14
happyReduction_14 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (Term happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  7 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Times happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  7 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Div happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  7 happyReduction_17
happyReduction_17 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Factor happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  8 happyReduction_18
happyReduction_18 _
	 =  HappyAbsSyn8
		 (Unit
	)

happyReduce_19 = happySpecReduce_1  8 happyReduction_19
happyReduction_19 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn8
		 (Int happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  8 happyReduction_20
happyReduction_20 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn8
		 (Bool happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  8 happyReduction_21
happyReduction_21 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn8
		 (Var happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happyReduce 5 8 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Pair happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_3  8 happyReduction_23
happyReduction_23 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Nested happy_var_2
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  9 happyReduction_24
happyReduction_24 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn9
		 (PVar happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happyReduce 5 9 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (PPair happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_1  9 happyReduction_26
happyReduction_26 _
	 =  HappyAbsSyn9
		 (PUnit
	)

happyReduce_27 = happySpecReduce_1  9 happyReduction_27
happyReduction_27 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn9
		 (PInt happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  9 happyReduction_28
happyReduction_28 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn9
		 (PBool happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  9 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (PNest happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 34 34 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLet -> cont 10;
	TokenIn -> cont 11;
	TokenIf -> cont 12;
	TokenThen -> cont 13;
	TokenElse -> cont 14;
	TokenBool happy_dollar_dollar -> cont 15;
	TokenInt happy_dollar_dollar -> cont 16;
	TokenVar happy_dollar_dollar -> cont 17;
	TokenUnit -> cont 18;
	TokenEq -> cont 19;
	TokenLam -> cont 20;
	TokenArrow -> cont 21;
	TokenPlus -> cont 22;
	TokenMinus -> cont 23;
	TokenTimes -> cont 24;
	TokenDiv -> cont 25;
	TokenLT -> cont 26;
	TokenGT -> cont 27;
	TokenLE -> cont 28;
	TokenGE -> cont 29;
	TokenEqEq -> cont 30;
	TokenOB -> cont 31;
	TokenCB -> cont 32;
	TokenComma -> cont 33;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 34 tk tks = happyError' (tks, explist)
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
        ("in"   , rest) -> TokenIn          : lexer rest
        ("if"   , rest) -> TokenIf          : lexer rest
        ("then" , rest) -> TokenThen        : lexer rest
        ("else" , rest) -> TokenElse        : lexer rest
        ("True" , rest) -> TokenBool True   : lexer rest
        ("False", rest) -> TokenBool False  : lexer rest
        (var  , rest) -> TokenVar var       : lexer rest
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
