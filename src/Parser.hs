{-# OPTIONS_GHC -w #-}
module Parser where

import Data.Char
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,164) ([58624,4098,128,0,8192,0,23680,512,0,0,49152,124,32768,1,0,0,741,16,0,0,0,0,0,0,8,10240,32791,36864,49163,0,8,60416,4098,28672,2048,14336,1024,7168,512,3584,256,1792,128,896,64,448,32,224,16,112,8,0,0,32,20480,46,1,768,0,384,0,192,0,96,0,48,0,24,0,12,0,0,0,0,2964,64,1482,32,0,0,0,0,189,49156,92,20482,46,10241,32791,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Exp","Exp1","AExp","Term","Factor","let","in","if","then","else","bool","int","var","'='","'$'","'->'","'+'","'-'","'*'","'/'","'<'","'>'","'<='","'>='","'=='","'('","')'","%eof"]
        bit_start = st Prelude.* 31
        bit_end = (st Prelude.+ 1) Prelude.* 31
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..30]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (9) = happyShift action_2
action_0 (11) = happyShift action_8
action_0 (14) = happyShift action_9
action_0 (15) = happyShift action_10
action_0 (16) = happyShift action_11
action_0 (18) = happyShift action_12
action_0 (29) = happyShift action_13
action_0 (4) = happyGoto action_3
action_0 (5) = happyGoto action_4
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (9) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (16) = happyShift action_27
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (11) = happyShift action_8
action_3 (14) = happyShift action_9
action_3 (15) = happyShift action_10
action_3 (16) = happyShift action_11
action_3 (18) = happyShift action_12
action_3 (29) = happyShift action_13
action_3 (31) = happyAccept
action_3 (5) = happyGoto action_26
action_3 (6) = happyGoto action_5
action_3 (7) = happyGoto action_6
action_3 (8) = happyGoto action_7
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_3

action_5 (20) = happyShift action_19
action_5 (21) = happyShift action_20
action_5 (24) = happyShift action_21
action_5 (25) = happyShift action_22
action_5 (26) = happyShift action_23
action_5 (27) = happyShift action_24
action_5 (28) = happyShift action_25
action_5 _ = happyReduce_6

action_6 (22) = happyShift action_17
action_6 (23) = happyShift action_18
action_6 _ = happyReduce_14

action_7 _ = happyReduce_17

action_8 (9) = happyShift action_2
action_8 (11) = happyShift action_8
action_8 (14) = happyShift action_9
action_8 (15) = happyShift action_10
action_8 (16) = happyShift action_11
action_8 (18) = happyShift action_12
action_8 (29) = happyShift action_13
action_8 (4) = happyGoto action_16
action_8 (5) = happyGoto action_4
action_8 (6) = happyGoto action_5
action_8 (7) = happyGoto action_6
action_8 (8) = happyGoto action_7
action_8 _ = happyFail (happyExpListPerState 8)

action_9 _ = happyReduce_19

action_10 _ = happyReduce_18

action_11 _ = happyReduce_20

action_12 (16) = happyShift action_15
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (9) = happyShift action_2
action_13 (11) = happyShift action_8
action_13 (14) = happyShift action_9
action_13 (15) = happyShift action_10
action_13 (16) = happyShift action_11
action_13 (18) = happyShift action_12
action_13 (29) = happyShift action_13
action_13 (4) = happyGoto action_14
action_13 (5) = happyGoto action_4
action_13 (6) = happyGoto action_5
action_13 (7) = happyGoto action_6
action_13 (8) = happyGoto action_7
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (11) = happyShift action_8
action_14 (14) = happyShift action_9
action_14 (15) = happyShift action_10
action_14 (16) = happyShift action_11
action_14 (18) = happyShift action_12
action_14 (29) = happyShift action_13
action_14 (30) = happyShift action_40
action_14 (5) = happyGoto action_26
action_14 (6) = happyGoto action_5
action_14 (7) = happyGoto action_6
action_14 (8) = happyGoto action_7
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (19) = happyShift action_39
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (11) = happyShift action_8
action_16 (12) = happyShift action_38
action_16 (14) = happyShift action_9
action_16 (15) = happyShift action_10
action_16 (16) = happyShift action_11
action_16 (18) = happyShift action_12
action_16 (29) = happyShift action_13
action_16 (5) = happyGoto action_26
action_16 (6) = happyGoto action_5
action_16 (7) = happyGoto action_6
action_16 (8) = happyGoto action_7
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (14) = happyShift action_9
action_17 (15) = happyShift action_10
action_17 (16) = happyShift action_11
action_17 (29) = happyShift action_13
action_17 (8) = happyGoto action_37
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (14) = happyShift action_9
action_18 (15) = happyShift action_10
action_18 (16) = happyShift action_11
action_18 (29) = happyShift action_13
action_18 (8) = happyGoto action_36
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (14) = happyShift action_9
action_19 (15) = happyShift action_10
action_19 (16) = happyShift action_11
action_19 (29) = happyShift action_13
action_19 (7) = happyGoto action_35
action_19 (8) = happyGoto action_7
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (14) = happyShift action_9
action_20 (15) = happyShift action_10
action_20 (16) = happyShift action_11
action_20 (29) = happyShift action_13
action_20 (7) = happyGoto action_34
action_20 (8) = happyGoto action_7
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (14) = happyShift action_9
action_21 (15) = happyShift action_10
action_21 (16) = happyShift action_11
action_21 (29) = happyShift action_13
action_21 (7) = happyGoto action_33
action_21 (8) = happyGoto action_7
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (14) = happyShift action_9
action_22 (15) = happyShift action_10
action_22 (16) = happyShift action_11
action_22 (29) = happyShift action_13
action_22 (7) = happyGoto action_32
action_22 (8) = happyGoto action_7
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (14) = happyShift action_9
action_23 (15) = happyShift action_10
action_23 (16) = happyShift action_11
action_23 (29) = happyShift action_13
action_23 (7) = happyGoto action_31
action_23 (8) = happyGoto action_7
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (14) = happyShift action_9
action_24 (15) = happyShift action_10
action_24 (16) = happyShift action_11
action_24 (29) = happyShift action_13
action_24 (7) = happyGoto action_30
action_24 (8) = happyGoto action_7
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (14) = happyShift action_9
action_25 (15) = happyShift action_10
action_25 (16) = happyShift action_11
action_25 (29) = happyShift action_13
action_25 (7) = happyGoto action_29
action_25 (8) = happyGoto action_7
action_25 _ = happyFail (happyExpListPerState 25)

action_26 _ = happyReduce_2

action_27 (17) = happyShift action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (9) = happyShift action_2
action_28 (11) = happyShift action_8
action_28 (14) = happyShift action_9
action_28 (15) = happyShift action_10
action_28 (16) = happyShift action_11
action_28 (18) = happyShift action_12
action_28 (29) = happyShift action_13
action_28 (4) = happyGoto action_43
action_28 (5) = happyGoto action_4
action_28 (6) = happyGoto action_5
action_28 (7) = happyGoto action_6
action_28 (8) = happyGoto action_7
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (22) = happyShift action_17
action_29 (23) = happyShift action_18
action_29 _ = happyReduce_13

action_30 (22) = happyShift action_17
action_30 (23) = happyShift action_18
action_30 _ = happyReduce_12

action_31 (22) = happyShift action_17
action_31 (23) = happyShift action_18
action_31 _ = happyReduce_11

action_32 (22) = happyShift action_17
action_32 (23) = happyShift action_18
action_32 _ = happyReduce_10

action_33 (22) = happyShift action_17
action_33 (23) = happyShift action_18
action_33 _ = happyReduce_9

action_34 (22) = happyShift action_17
action_34 (23) = happyShift action_18
action_34 _ = happyReduce_8

action_35 (22) = happyShift action_17
action_35 (23) = happyShift action_18
action_35 _ = happyReduce_7

action_36 _ = happyReduce_16

action_37 _ = happyReduce_15

action_38 (9) = happyShift action_2
action_38 (11) = happyShift action_8
action_38 (14) = happyShift action_9
action_38 (15) = happyShift action_10
action_38 (16) = happyShift action_11
action_38 (18) = happyShift action_12
action_38 (29) = happyShift action_13
action_38 (4) = happyGoto action_42
action_38 (5) = happyGoto action_4
action_38 (6) = happyGoto action_5
action_38 (7) = happyGoto action_6
action_38 (8) = happyGoto action_7
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (9) = happyShift action_2
action_39 (11) = happyShift action_8
action_39 (14) = happyShift action_9
action_39 (15) = happyShift action_10
action_39 (16) = happyShift action_11
action_39 (18) = happyShift action_12
action_39 (29) = happyShift action_13
action_39 (4) = happyGoto action_41
action_39 (5) = happyGoto action_4
action_39 (6) = happyGoto action_5
action_39 (7) = happyGoto action_6
action_39 (8) = happyGoto action_7
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_21

action_41 (11) = happyShift action_8
action_41 (14) = happyShift action_9
action_41 (15) = happyShift action_10
action_41 (16) = happyShift action_11
action_41 (18) = happyShift action_12
action_41 (29) = happyShift action_13
action_41 (5) = happyGoto action_26
action_41 (6) = happyGoto action_5
action_41 (7) = happyGoto action_6
action_41 (8) = happyGoto action_7
action_41 _ = happyReduce_5

action_42 (11) = happyShift action_8
action_42 (13) = happyShift action_45
action_42 (14) = happyShift action_9
action_42 (15) = happyShift action_10
action_42 (16) = happyShift action_11
action_42 (18) = happyShift action_12
action_42 (29) = happyShift action_13
action_42 (5) = happyGoto action_26
action_42 (6) = happyGoto action_5
action_42 (7) = happyGoto action_6
action_42 (8) = happyGoto action_7
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (10) = happyShift action_44
action_43 (11) = happyShift action_8
action_43 (14) = happyShift action_9
action_43 (15) = happyShift action_10
action_43 (16) = happyShift action_11
action_43 (18) = happyShift action_12
action_43 (29) = happyShift action_13
action_43 (5) = happyGoto action_26
action_43 (6) = happyGoto action_5
action_43 (7) = happyGoto action_6
action_43 (8) = happyGoto action_7
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (9) = happyShift action_2
action_44 (11) = happyShift action_8
action_44 (14) = happyShift action_9
action_44 (15) = happyShift action_10
action_44 (16) = happyShift action_11
action_44 (18) = happyShift action_12
action_44 (29) = happyShift action_13
action_44 (4) = happyGoto action_47
action_44 (5) = happyGoto action_4
action_44 (6) = happyGoto action_5
action_44 (7) = happyGoto action_6
action_44 (8) = happyGoto action_7
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (9) = happyShift action_2
action_45 (11) = happyShift action_8
action_45 (14) = happyShift action_9
action_45 (15) = happyShift action_10
action_45 (16) = happyShift action_11
action_45 (18) = happyShift action_12
action_45 (29) = happyShift action_13
action_45 (4) = happyGoto action_46
action_45 (5) = happyGoto action_4
action_45 (6) = happyGoto action_5
action_45 (7) = happyGoto action_6
action_45 (8) = happyGoto action_7
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (11) = happyShift action_8
action_46 (14) = happyShift action_9
action_46 (15) = happyShift action_10
action_46 (16) = happyShift action_11
action_46 (18) = happyShift action_12
action_46 (29) = happyShift action_13
action_46 (5) = happyGoto action_26
action_46 (6) = happyGoto action_5
action_46 (7) = happyGoto action_6
action_46 (8) = happyGoto action_7
action_46 _ = happyReduce_4

action_47 (11) = happyShift action_8
action_47 (14) = happyShift action_9
action_47 (15) = happyShift action_10
action_47 (16) = happyShift action_11
action_47 (18) = happyShift action_12
action_47 (29) = happyShift action_13
action_47 (5) = happyGoto action_26
action_47 (6) = happyGoto action_5
action_47 (7) = happyGoto action_6
action_47 (8) = happyGoto action_7
action_47 _ = happyReduce_1

happyReduce_1 = happyReduce 6 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
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
	(HappyTerminal (TokenVar happy_var_2)) `HappyStk`
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
happyReduction_18 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn8
		 (Int happy_var_1
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  8 happyReduction_19
happyReduction_19 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn8
		 (Bool happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  8 happyReduction_20
happyReduction_20 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn8
		 (Var happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  8 happyReduction_21
happyReduction_21 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (Nested happy_var_2
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 31 31 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLet -> cont 9;
	TokenIn -> cont 10;
	TokenIf -> cont 11;
	TokenThen -> cont 12;
	TokenElse -> cont 13;
	TokenBool happy_dollar_dollar -> cont 14;
	TokenInt happy_dollar_dollar -> cont 15;
	TokenVar happy_dollar_dollar -> cont 16;
	TokenEq -> cont 17;
	TokenLam -> cont 18;
	TokenArrow -> cont 19;
	TokenPlus -> cont 20;
	TokenMinus -> cont 21;
	TokenTimes -> cont 22;
	TokenDiv -> cont 23;
	TokenLT -> cont 24;
	TokenGT -> cont 25;
	TokenLE -> cont 26;
	TokenGE -> cont 27;
	TokenEqEq -> cont 28;
	TokenOB -> cont 29;
	TokenCB -> cont 30;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 31 tk tks = happyError' (tks, explist)
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
    = Let Identifier Exp Exp
    | Application Exp Exp1
    | Exp1 Exp1
    deriving Show

data Exp1
    = IfThenElse Exp Exp Exp
    | Lambda Identifier Exp
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
    = Int Int
    | Bool Bool
    | Var String
    | Nested Exp
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
    deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
    | isDigit c = lexNum (c:cs)
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
