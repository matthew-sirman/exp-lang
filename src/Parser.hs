{-# OPTIONS_GHC -w #-}
module Parser (
    Exp(..)
  , ArgList(..)
  , Exp1(..)
  , AExp(..)
  , Term(..)
  , Factor(..)
  , Pat(..)
  , runLexer
  , parse
)where

import Data.Char
import Control.Monad.State
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,238) ([9216,47,16385,0,0,3840,256,61952,4098,0,0,0,38912,15,0,6,0,0,2048,15,16385,754,16,0,0,0,0,0,0,0,0,0,3840,256,62016,4098,8192,47,7,1024,0,0,0,0,0,0,0,0,0,0,3840,256,62976,4098,0,15,1,64,0,3840,256,61440,4096,0,15,1,240,16,3840,256,61440,4096,0,15,1,240,16,3840,256,0,0,0,1536,0,24576,0,0,6,0,96,0,1536,0,24576,0,0,6,0,0,0,0,0,240,16,4096,0,61440,4096,9216,47,1,0,96,12068,256,0,0,9216,47,1,754,48,0,0,0,0,0,15,1,762,16,0,0,62016,4098,0,16,16384,754,16,12080,256,62016,4098,0,0,2,0,0,0,0,0,0,9216,47,1,755,16,12068,256,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Exp","ArgList","Exp1","AExp","Term","Factor","Pat","let","rec","in","if","then","else","bool","int","var","'()'","'='","'$'","'->'","'+'","'-'","'*'","'/'","'<'","'>'","'<='","'>='","'=='","'('","')'","','","%eof"]
        bit_start = st Prelude.* 36
        bit_end = (st Prelude.+ 1) Prelude.* 36
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..35]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (11) = happyShift action_8
action_0 (14) = happyShift action_9
action_0 (17) = happyShift action_10
action_0 (18) = happyShift action_11
action_0 (19) = happyShift action_12
action_0 (20) = happyShift action_13
action_0 (22) = happyShift action_14
action_0 (33) = happyShift action_15
action_0 (4) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 (7) = happyGoto action_5
action_0 (8) = happyGoto action_6
action_0 (9) = happyGoto action_7
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (11) = happyShift action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (17) = happyShift action_18
action_2 (18) = happyShift action_19
action_2 (19) = happyShift action_20
action_2 (20) = happyShift action_21
action_2 (33) = happyShift action_22
action_2 (10) = happyGoto action_24
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (14) = happyShift action_9
action_3 (17) = happyShift action_10
action_3 (18) = happyShift action_11
action_3 (19) = happyShift action_12
action_3 (20) = happyShift action_13
action_3 (22) = happyShift action_14
action_3 (33) = happyShift action_15
action_3 (36) = happyAccept
action_3 (6) = happyGoto action_35
action_3 (7) = happyGoto action_5
action_3 (8) = happyGoto action_6
action_3 (9) = happyGoto action_7
action_3 _ = happyFail (happyExpListPerState 3)

action_4 _ = happyReduce_4

action_5 (24) = happyShift action_28
action_5 (25) = happyShift action_29
action_5 (28) = happyShift action_30
action_5 (29) = happyShift action_31
action_5 (30) = happyShift action_32
action_5 (31) = happyShift action_33
action_5 (32) = happyShift action_34
action_5 _ = happyReduce_9

action_6 (26) = happyShift action_26
action_6 (27) = happyShift action_27
action_6 _ = happyReduce_17

action_7 _ = happyReduce_20

action_8 (12) = happyShift action_25
action_8 (17) = happyShift action_18
action_8 (18) = happyShift action_19
action_8 (19) = happyShift action_20
action_8 (20) = happyShift action_21
action_8 (33) = happyShift action_22
action_8 (10) = happyGoto action_24
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (11) = happyShift action_8
action_9 (14) = happyShift action_9
action_9 (17) = happyShift action_10
action_9 (18) = happyShift action_11
action_9 (19) = happyShift action_12
action_9 (20) = happyShift action_13
action_9 (22) = happyShift action_14
action_9 (33) = happyShift action_15
action_9 (4) = happyGoto action_23
action_9 (6) = happyGoto action_4
action_9 (7) = happyGoto action_5
action_9 (8) = happyGoto action_6
action_9 (9) = happyGoto action_7
action_9 _ = happyFail (happyExpListPerState 9)

action_10 _ = happyReduce_23

action_11 _ = happyReduce_22

action_12 _ = happyReduce_24

action_13 _ = happyReduce_21

action_14 (17) = happyShift action_18
action_14 (18) = happyShift action_19
action_14 (19) = happyShift action_20
action_14 (20) = happyShift action_21
action_14 (33) = happyShift action_22
action_14 (10) = happyGoto action_17
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (11) = happyShift action_8
action_15 (14) = happyShift action_9
action_15 (17) = happyShift action_10
action_15 (18) = happyShift action_11
action_15 (19) = happyShift action_12
action_15 (20) = happyShift action_13
action_15 (22) = happyShift action_14
action_15 (33) = happyShift action_15
action_15 (4) = happyGoto action_16
action_15 (6) = happyGoto action_4
action_15 (7) = happyGoto action_5
action_15 (8) = happyGoto action_6
action_15 (9) = happyGoto action_7
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (14) = happyShift action_9
action_16 (17) = happyShift action_10
action_16 (18) = happyShift action_11
action_16 (19) = happyShift action_12
action_16 (20) = happyShift action_13
action_16 (22) = happyShift action_14
action_16 (33) = happyShift action_15
action_16 (34) = happyShift action_51
action_16 (35) = happyShift action_52
action_16 (6) = happyGoto action_35
action_16 (7) = happyGoto action_5
action_16 (8) = happyGoto action_6
action_16 (9) = happyGoto action_7
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (23) = happyShift action_50
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_31

action_19 _ = happyReduce_30

action_20 _ = happyReduce_27

action_21 _ = happyReduce_29

action_22 (17) = happyShift action_18
action_22 (18) = happyShift action_19
action_22 (19) = happyShift action_20
action_22 (20) = happyShift action_21
action_22 (33) = happyShift action_22
action_22 (10) = happyGoto action_49
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (14) = happyShift action_9
action_23 (15) = happyShift action_48
action_23 (17) = happyShift action_10
action_23 (18) = happyShift action_11
action_23 (19) = happyShift action_12
action_23 (20) = happyShift action_13
action_23 (22) = happyShift action_14
action_23 (33) = happyShift action_15
action_23 (6) = happyGoto action_35
action_23 (7) = happyGoto action_5
action_23 (8) = happyGoto action_6
action_23 (9) = happyGoto action_7
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (17) = happyShift action_18
action_24 (18) = happyShift action_19
action_24 (19) = happyShift action_20
action_24 (20) = happyShift action_21
action_24 (33) = happyShift action_22
action_24 (5) = happyGoto action_46
action_24 (10) = happyGoto action_47
action_24 _ = happyReduce_5

action_25 (19) = happyShift action_45
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (17) = happyShift action_10
action_26 (18) = happyShift action_11
action_26 (19) = happyShift action_12
action_26 (20) = happyShift action_13
action_26 (33) = happyShift action_15
action_26 (9) = happyGoto action_44
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (17) = happyShift action_10
action_27 (18) = happyShift action_11
action_27 (19) = happyShift action_12
action_27 (20) = happyShift action_13
action_27 (33) = happyShift action_15
action_27 (9) = happyGoto action_43
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (17) = happyShift action_10
action_28 (18) = happyShift action_11
action_28 (19) = happyShift action_12
action_28 (20) = happyShift action_13
action_28 (33) = happyShift action_15
action_28 (8) = happyGoto action_42
action_28 (9) = happyGoto action_7
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (17) = happyShift action_10
action_29 (18) = happyShift action_11
action_29 (19) = happyShift action_12
action_29 (20) = happyShift action_13
action_29 (33) = happyShift action_15
action_29 (8) = happyGoto action_41
action_29 (9) = happyGoto action_7
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (17) = happyShift action_10
action_30 (18) = happyShift action_11
action_30 (19) = happyShift action_12
action_30 (20) = happyShift action_13
action_30 (33) = happyShift action_15
action_30 (8) = happyGoto action_40
action_30 (9) = happyGoto action_7
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (17) = happyShift action_10
action_31 (18) = happyShift action_11
action_31 (19) = happyShift action_12
action_31 (20) = happyShift action_13
action_31 (33) = happyShift action_15
action_31 (8) = happyGoto action_39
action_31 (9) = happyGoto action_7
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (17) = happyShift action_10
action_32 (18) = happyShift action_11
action_32 (19) = happyShift action_12
action_32 (20) = happyShift action_13
action_32 (33) = happyShift action_15
action_32 (8) = happyGoto action_38
action_32 (9) = happyGoto action_7
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (17) = happyShift action_10
action_33 (18) = happyShift action_11
action_33 (19) = happyShift action_12
action_33 (20) = happyShift action_13
action_33 (33) = happyShift action_15
action_33 (8) = happyGoto action_37
action_33 (9) = happyGoto action_7
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (17) = happyShift action_10
action_34 (18) = happyShift action_11
action_34 (19) = happyShift action_12
action_34 (20) = happyShift action_13
action_34 (33) = happyShift action_15
action_34 (8) = happyGoto action_36
action_34 (9) = happyGoto action_7
action_34 _ = happyFail (happyExpListPerState 34)

action_35 _ = happyReduce_3

action_36 (26) = happyShift action_26
action_36 (27) = happyShift action_27
action_36 _ = happyReduce_16

action_37 (26) = happyShift action_26
action_37 (27) = happyShift action_27
action_37 _ = happyReduce_15

action_38 (26) = happyShift action_26
action_38 (27) = happyShift action_27
action_38 _ = happyReduce_14

action_39 (26) = happyShift action_26
action_39 (27) = happyShift action_27
action_39 _ = happyReduce_13

action_40 (26) = happyShift action_26
action_40 (27) = happyShift action_27
action_40 _ = happyReduce_12

action_41 (26) = happyShift action_26
action_41 (27) = happyShift action_27
action_41 _ = happyReduce_11

action_42 (26) = happyShift action_26
action_42 (27) = happyShift action_27
action_42 _ = happyReduce_10

action_43 _ = happyReduce_19

action_44 _ = happyReduce_18

action_45 (17) = happyShift action_18
action_45 (18) = happyShift action_19
action_45 (19) = happyShift action_20
action_45 (20) = happyShift action_21
action_45 (33) = happyShift action_22
action_45 (5) = happyGoto action_60
action_45 (10) = happyGoto action_47
action_45 _ = happyReduce_5

action_46 (21) = happyShift action_59
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (17) = happyShift action_18
action_47 (18) = happyShift action_19
action_47 (19) = happyShift action_20
action_47 (20) = happyShift action_21
action_47 (33) = happyShift action_22
action_47 (5) = happyGoto action_58
action_47 (10) = happyGoto action_47
action_47 _ = happyReduce_5

action_48 (11) = happyShift action_8
action_48 (14) = happyShift action_9
action_48 (17) = happyShift action_10
action_48 (18) = happyShift action_11
action_48 (19) = happyShift action_12
action_48 (20) = happyShift action_13
action_48 (22) = happyShift action_14
action_48 (33) = happyShift action_15
action_48 (4) = happyGoto action_57
action_48 (6) = happyGoto action_4
action_48 (7) = happyGoto action_5
action_48 (8) = happyGoto action_6
action_48 (9) = happyGoto action_7
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (34) = happyShift action_55
action_49 (35) = happyShift action_56
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (11) = happyShift action_8
action_50 (14) = happyShift action_9
action_50 (17) = happyShift action_10
action_50 (18) = happyShift action_11
action_50 (19) = happyShift action_12
action_50 (20) = happyShift action_13
action_50 (22) = happyShift action_14
action_50 (33) = happyShift action_15
action_50 (4) = happyGoto action_54
action_50 (6) = happyGoto action_4
action_50 (7) = happyGoto action_5
action_50 (8) = happyGoto action_6
action_50 (9) = happyGoto action_7
action_50 _ = happyFail (happyExpListPerState 50)

action_51 _ = happyReduce_26

action_52 (11) = happyShift action_8
action_52 (14) = happyShift action_9
action_52 (17) = happyShift action_10
action_52 (18) = happyShift action_11
action_52 (19) = happyShift action_12
action_52 (20) = happyShift action_13
action_52 (22) = happyShift action_14
action_52 (33) = happyShift action_15
action_52 (4) = happyGoto action_53
action_52 (6) = happyGoto action_4
action_52 (7) = happyGoto action_5
action_52 (8) = happyGoto action_6
action_52 (9) = happyGoto action_7
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (14) = happyShift action_9
action_53 (17) = happyShift action_10
action_53 (18) = happyShift action_11
action_53 (19) = happyShift action_12
action_53 (20) = happyShift action_13
action_53 (22) = happyShift action_14
action_53 (33) = happyShift action_15
action_53 (34) = happyShift action_65
action_53 (6) = happyGoto action_35
action_53 (7) = happyGoto action_5
action_53 (8) = happyGoto action_6
action_53 (9) = happyGoto action_7
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (14) = happyShift action_9
action_54 (17) = happyShift action_10
action_54 (18) = happyShift action_11
action_54 (19) = happyShift action_12
action_54 (20) = happyShift action_13
action_54 (22) = happyShift action_14
action_54 (33) = happyShift action_15
action_54 (6) = happyGoto action_35
action_54 (7) = happyGoto action_5
action_54 (8) = happyGoto action_6
action_54 (9) = happyGoto action_7
action_54 _ = happyReduce_8

action_55 _ = happyReduce_32

action_56 (17) = happyShift action_18
action_56 (18) = happyShift action_19
action_56 (19) = happyShift action_20
action_56 (20) = happyShift action_21
action_56 (33) = happyShift action_22
action_56 (10) = happyGoto action_64
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (14) = happyShift action_9
action_57 (16) = happyShift action_63
action_57 (17) = happyShift action_10
action_57 (18) = happyShift action_11
action_57 (19) = happyShift action_12
action_57 (20) = happyShift action_13
action_57 (22) = happyShift action_14
action_57 (33) = happyShift action_15
action_57 (6) = happyGoto action_35
action_57 (7) = happyGoto action_5
action_57 (8) = happyGoto action_6
action_57 (9) = happyGoto action_7
action_57 _ = happyFail (happyExpListPerState 57)

action_58 _ = happyReduce_6

action_59 (11) = happyShift action_8
action_59 (14) = happyShift action_9
action_59 (17) = happyShift action_10
action_59 (18) = happyShift action_11
action_59 (19) = happyShift action_12
action_59 (20) = happyShift action_13
action_59 (22) = happyShift action_14
action_59 (33) = happyShift action_15
action_59 (4) = happyGoto action_62
action_59 (6) = happyGoto action_4
action_59 (7) = happyGoto action_5
action_59 (8) = happyGoto action_6
action_59 (9) = happyGoto action_7
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (21) = happyShift action_61
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (11) = happyShift action_8
action_61 (14) = happyShift action_9
action_61 (17) = happyShift action_10
action_61 (18) = happyShift action_11
action_61 (19) = happyShift action_12
action_61 (20) = happyShift action_13
action_61 (22) = happyShift action_14
action_61 (33) = happyShift action_15
action_61 (4) = happyGoto action_69
action_61 (6) = happyGoto action_4
action_61 (7) = happyGoto action_5
action_61 (8) = happyGoto action_6
action_61 (9) = happyGoto action_7
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (13) = happyShift action_68
action_62 (14) = happyShift action_9
action_62 (17) = happyShift action_10
action_62 (18) = happyShift action_11
action_62 (19) = happyShift action_12
action_62 (20) = happyShift action_13
action_62 (22) = happyShift action_14
action_62 (33) = happyShift action_15
action_62 (6) = happyGoto action_35
action_62 (7) = happyGoto action_5
action_62 (8) = happyGoto action_6
action_62 (9) = happyGoto action_7
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (11) = happyShift action_8
action_63 (14) = happyShift action_9
action_63 (17) = happyShift action_10
action_63 (18) = happyShift action_11
action_63 (19) = happyShift action_12
action_63 (20) = happyShift action_13
action_63 (22) = happyShift action_14
action_63 (33) = happyShift action_15
action_63 (4) = happyGoto action_67
action_63 (6) = happyGoto action_4
action_63 (7) = happyGoto action_5
action_63 (8) = happyGoto action_6
action_63 (9) = happyGoto action_7
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (34) = happyShift action_66
action_64 _ = happyFail (happyExpListPerState 64)

action_65 _ = happyReduce_25

action_66 _ = happyReduce_28

action_67 (14) = happyShift action_9
action_67 (17) = happyShift action_10
action_67 (18) = happyShift action_11
action_67 (19) = happyShift action_12
action_67 (20) = happyShift action_13
action_67 (22) = happyShift action_14
action_67 (33) = happyShift action_15
action_67 (6) = happyGoto action_35
action_67 (7) = happyGoto action_5
action_67 (8) = happyGoto action_6
action_67 (9) = happyGoto action_7
action_67 _ = happyReduce_7

action_68 (11) = happyShift action_8
action_68 (14) = happyShift action_9
action_68 (17) = happyShift action_10
action_68 (18) = happyShift action_11
action_68 (19) = happyShift action_12
action_68 (20) = happyShift action_13
action_68 (22) = happyShift action_14
action_68 (33) = happyShift action_15
action_68 (4) = happyGoto action_71
action_68 (6) = happyGoto action_4
action_68 (7) = happyGoto action_5
action_68 (8) = happyGoto action_6
action_68 (9) = happyGoto action_7
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (13) = happyShift action_70
action_69 (14) = happyShift action_9
action_69 (17) = happyShift action_10
action_69 (18) = happyShift action_11
action_69 (19) = happyShift action_12
action_69 (20) = happyShift action_13
action_69 (22) = happyShift action_14
action_69 (33) = happyShift action_15
action_69 (6) = happyGoto action_35
action_69 (7) = happyGoto action_5
action_69 (8) = happyGoto action_6
action_69 (9) = happyGoto action_7
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (11) = happyShift action_8
action_70 (14) = happyShift action_9
action_70 (17) = happyShift action_10
action_70 (18) = happyShift action_11
action_70 (19) = happyShift action_12
action_70 (20) = happyShift action_13
action_70 (22) = happyShift action_14
action_70 (33) = happyShift action_15
action_70 (4) = happyGoto action_72
action_70 (6) = happyGoto action_4
action_70 (7) = happyGoto action_5
action_70 (8) = happyGoto action_6
action_70 (9) = happyGoto action_7
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (14) = happyShift action_9
action_71 (17) = happyShift action_10
action_71 (18) = happyShift action_11
action_71 (19) = happyShift action_12
action_71 (20) = happyShift action_13
action_71 (22) = happyShift action_14
action_71 (33) = happyShift action_15
action_71 (6) = happyGoto action_35
action_71 (7) = happyGoto action_5
action_71 (8) = happyGoto action_6
action_71 (9) = happyGoto action_7
action_71 _ = happyReduce_1

action_72 (14) = happyShift action_9
action_72 (17) = happyShift action_10
action_72 (18) = happyShift action_11
action_72 (19) = happyShift action_12
action_72 (20) = happyShift action_13
action_72 (22) = happyShift action_14
action_72 (33) = happyShift action_15
action_72 (6) = happyGoto action_35
action_72 (7) = happyGoto action_5
action_72 (8) = happyGoto action_6
action_72 (9) = happyGoto action_7
action_72 _ = happyReduce_2

happyReduce_1 = happyReduce 7 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 8 4 happyReduction_2
happyReduction_2 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyTerminal (TokenVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Rec happy_var_3 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  4 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Application happy_var_1 happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (Exp1 happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  5 happyReduction_5
happyReduction_5  =  HappyAbsSyn5
		 (ArgEmpty
	)

happyReduce_6 = happySpecReduce_2  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn5
		 (ArgCons happy_var_1 happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happyReduce 6 6 happyReduction_7
happyReduction_7 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (IfThenElse happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 4 6 happyReduction_8
happyReduction_8 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Lambda happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_1  6 happyReduction_9
happyReduction_9 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 (ArithExp happy_var_1
	)
happyReduction_9 _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  7 happyReduction_10
happyReduction_10 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  7 happyReduction_11
happyReduction_11 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  7 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (LessThan happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  7 happyReduction_13
happyReduction_13 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (GreaterThan happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  7 happyReduction_14
happyReduction_14 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (LessEqual happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  7 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (GreaterEqual happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  7 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (EqualTo happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  7 happyReduction_17
happyReduction_17 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Term happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  8 happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Times happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  8 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (Div happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  8 happyReduction_20
happyReduction_20 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (Factor happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  9 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn9
		 (Unit
	)

happyReduce_22 = happySpecReduce_1  9 happyReduction_22
happyReduction_22 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn9
		 (Int happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  9 happyReduction_23
happyReduction_23 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn9
		 (Bool happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  9 happyReduction_24
happyReduction_24 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn9
		 (Var happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happyReduce 5 9 happyReduction_25
happyReduction_25 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (Pair happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_26 = happySpecReduce_3  9 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Nested happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  10 happyReduction_27
happyReduction_27 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn10
		 (PVar happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happyReduce 5 10 happyReduction_28
happyReduction_28 (_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (PPair happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_1  10 happyReduction_29
happyReduction_29 _
	 =  HappyAbsSyn10
		 (PUnit
	)

happyReduce_30 = happySpecReduce_1  10 happyReduction_30
happyReduction_30 (HappyTerminal (TokenInt happy_var_1))
	 =  HappyAbsSyn10
		 (PInt happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  10 happyReduction_31
happyReduction_31 (HappyTerminal (TokenBool happy_var_1))
	 =  HappyAbsSyn10
		 (PBool happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  10 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (PNest happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 36 36 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenLet -> cont 11;
	TokenRec -> cont 12;
	TokenIn -> cont 13;
	TokenIf -> cont 14;
	TokenThen -> cont 15;
	TokenElse -> cont 16;
	TokenBool happy_dollar_dollar -> cont 17;
	TokenInt happy_dollar_dollar -> cont 18;
	TokenVar happy_dollar_dollar -> cont 19;
	TokenUnit -> cont 20;
	TokenEq -> cont 21;
	TokenLam -> cont 22;
	TokenArrow -> cont 23;
	TokenPlus -> cont 24;
	TokenMinus -> cont 25;
	TokenTimes -> cont 26;
	TokenDiv -> cont 27;
	TokenLT -> cont 28;
	TokenGT -> cont 29;
	TokenLE -> cont 30;
	TokenGE -> cont 31;
	TokenEqEq -> cont 32;
	TokenOB -> cont 33;
	TokenCB -> cont 34;
	TokenComma -> cont 35;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 36 tk tks = happyError' (tks, explist)
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
    = Let Pat ArgList Exp Exp
    | Rec Identifier ArgList Exp Exp
    | Application Exp Exp1
    | Exp1 Exp1
    deriving Show

data ArgList
    = ArgEmpty
    | ArgCons Pat ArgList
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

data LexerState = LexerState
    { inComment :: Bool
    }

runLexer :: String -> [Token]
runLexer source = evalState (lexer source) emptyLexState
    where
        emptyLexState = LexerState
            { inComment = False
            }

lexer :: String -> State LexerState [Token]
lexer [] = pure []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
    | isDigit c = lexNum (c:cs)
lexer ('(':'*':cs)  = do
    modify setComment
    lexer cs
    where
        setComment s = s { inComment = True }
lexer ('*':')':cs)  = do
    modify unsetComment
    lexer cs
    where
        unsetComment s = s { inComment = False }
lexer ('(':')':cs)  = add TokenUnit  cs
lexer ('=':'=':cs)  = add TokenEqEq  cs
lexer ('=':cs)      = add TokenEq    cs
lexer ('$':cs)      = add TokenLam   cs
lexer ('-':'>':cs)  = add TokenArrow cs
lexer ('+':cs)      = add TokenPlus  cs
lexer ('-':cs)      = add TokenMinus cs
lexer ('*':cs)      = add TokenTimes cs
lexer ('/':cs)      = add TokenDiv   cs
lexer ('<':'=':cs)  = add TokenLE    cs
lexer ('>':'=':cs)  = add TokenGE    cs
lexer ('<':cs)      = add TokenLT    cs
lexer ('>':cs)      = add TokenGT    cs
lexer ('(':cs)      = add TokenOB    cs
lexer (')':cs)      = add TokenCB    cs
lexer (',':cs)      = add TokenComma cs

lexNum cs = add (TokenInt (read num)) rest
    where (num, rest) = span isDigit cs

lexVar cs =
    case span isAlphaNum cs of
        ("let"  , rest) -> add TokenLet             rest
        ("rec"  , rest) -> add TokenRec             rest
        ("in"   , rest) -> add TokenIn              rest
        ("if"   , rest) -> add TokenIf              rest
        ("then" , rest) -> add TokenThen            rest
        ("else" , rest) -> add TokenElse            rest
        ("True" , rest) -> add (TokenBool True)     rest
        ("False", rest) -> add (TokenBool False)    rest
        (var    , rest) -> add (TokenVar var)       rest

add :: Token -> String -> State LexerState [Token]
add t cs = do
    comment <- gets inComment
    rest <- lexer cs
    if comment then
        pure rest
    else
        pure $ t : rest
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
