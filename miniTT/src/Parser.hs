{-# OPTIONS_GHC -w #-}
module Parser ( parseExpr ) where
import Lexer
import Syntax
import Control.Monad.Except
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,381) ([16384,16393,0,0,0,0,0,0,544,0,2048,0,4608,128,0,0,0,72,2,0,0,0,130,0,4352,0,1184,32,45824,39344,0,32786,0,0,0,32128,27897,0,16393,0,24934,307,0,0,38912,52613,4,45235,153,4608,128,16384,4098,0,64,0,2048,0,0,0,0,0,0,0,0,0,0,24576,13846,19,0,0,32128,27897,0,0,0,64,0,0,0,0,544,0,17408,0,30560,6974,0,0,0,8256,32,8192,4,0,0,49152,27692,38,34200,1229,0,12288,24576,13846,19,49868,614,16384,0,0,8,0,0,0,0,0,55296,53141,6,61627,217,5728,4918,0,0,32768,55385,76,2864,2459,26112,13153,1,64,0,0,0,0,0,24576,62999,19,49868,614,23936,27897,45056,48939,13,32784,0,11456,9836,0,0,0,45235,157,1024,0,0,0,32768,63837,108,11184,3487,30208,46053,49153,31790,54,0,384,0,0,24576,30230,19,51948,879,22912,19672,0,2,0,0,0,44736,14076,0,1,0,0,0,4864,128,32768,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Prog","Expr","ColonExprDot","Pat","Choices","Choices_","ChoisesWhite","ChoisesArrow","Decl","lambda","recUnit","rec","'\8594'","str","constr","'='","'('","')'","'.'","':'","';'","'\928'","'\931'","','","fun","sum","'|'","'_'","zero","one","two","'\215'","'U'","%eof"]
        bit_start = st * 37
        bit_end = (st + 1) * 37
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..36]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (15) = happyShift action_5
action_0 (17) = happyShift action_6
action_0 (20) = happyShift action_7
action_0 (31) = happyShift action_8
action_0 (4) = happyGoto action_2
action_0 (7) = happyGoto action_3
action_0 (12) = happyGoto action_4
action_0 _ = happyReduce_1

action_1 _ = happyFail (happyExpListPerState 1)

action_2 (37) = happyAccept
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (23) = happyShift action_12
action_3 (27) = happyShift action_13
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (24) = happyShift action_11
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (17) = happyShift action_6
action_5 (20) = happyShift action_7
action_5 (31) = happyShift action_8
action_5 (7) = happyGoto action_10
action_5 _ = happyFail (happyExpListPerState 5)

action_6 _ = happyReduce_23

action_7 (17) = happyShift action_6
action_7 (20) = happyShift action_7
action_7 (31) = happyShift action_8
action_7 (7) = happyGoto action_9
action_7 _ = happyFail (happyExpListPerState 7)

action_8 _ = happyReduce_25

action_9 (21) = happyShift action_30
action_9 (27) = happyShift action_13
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (23) = happyShift action_29
action_10 (27) = happyShift action_13
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (15) = happyShift action_5
action_11 (17) = happyShift action_6
action_11 (20) = happyShift action_7
action_11 (31) = happyShift action_8
action_11 (4) = happyGoto action_28
action_11 (7) = happyGoto action_3
action_11 (12) = happyGoto action_4
action_11 _ = happyReduce_1

action_12 (13) = happyShift action_16
action_12 (14) = happyShift action_17
action_12 (17) = happyShift action_18
action_12 (18) = happyShift action_19
action_12 (20) = happyShift action_20
action_12 (25) = happyShift action_21
action_12 (26) = happyShift action_22
action_12 (28) = happyShift action_23
action_12 (29) = happyShift action_24
action_12 (32) = happyShift action_25
action_12 (33) = happyShift action_26
action_12 (36) = happyShift action_27
action_12 (5) = happyGoto action_15
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (17) = happyShift action_6
action_13 (20) = happyShift action_7
action_13 (31) = happyShift action_8
action_13 (7) = happyGoto action_14
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_24

action_15 (13) = happyShift action_16
action_15 (14) = happyShift action_17
action_15 (16) = happyShift action_42
action_15 (17) = happyShift action_18
action_15 (18) = happyShift action_19
action_15 (19) = happyShift action_43
action_15 (20) = happyShift action_20
action_15 (22) = happyShift action_44
action_15 (25) = happyShift action_21
action_15 (26) = happyShift action_22
action_15 (27) = happyShift action_45
action_15 (28) = happyShift action_23
action_15 (29) = happyShift action_24
action_15 (32) = happyShift action_25
action_15 (33) = happyShift action_26
action_15 (35) = happyShift action_46
action_15 (36) = happyShift action_27
action_15 (5) = happyGoto action_41
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (17) = happyShift action_6
action_16 (20) = happyShift action_7
action_16 (31) = happyShift action_8
action_16 (7) = happyGoto action_40
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (13) = happyShift action_16
action_17 (14) = happyShift action_17
action_17 (17) = happyShift action_18
action_17 (18) = happyShift action_19
action_17 (20) = happyShift action_20
action_17 (25) = happyShift action_21
action_17 (26) = happyShift action_22
action_17 (28) = happyShift action_23
action_17 (29) = happyShift action_24
action_17 (32) = happyShift action_25
action_17 (33) = happyShift action_26
action_17 (36) = happyShift action_27
action_17 (5) = happyGoto action_39
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_4

action_19 (13) = happyShift action_16
action_19 (14) = happyShift action_17
action_19 (17) = happyShift action_18
action_19 (18) = happyShift action_19
action_19 (20) = happyShift action_20
action_19 (25) = happyShift action_21
action_19 (26) = happyShift action_22
action_19 (28) = happyShift action_23
action_19 (29) = happyShift action_24
action_19 (32) = happyShift action_25
action_19 (33) = happyShift action_26
action_19 (36) = happyShift action_27
action_19 (5) = happyGoto action_38
action_19 _ = happyReduce_6

action_20 (13) = happyShift action_16
action_20 (14) = happyShift action_17
action_20 (17) = happyShift action_18
action_20 (18) = happyShift action_19
action_20 (20) = happyShift action_20
action_20 (25) = happyShift action_21
action_20 (26) = happyShift action_22
action_20 (28) = happyShift action_23
action_20 (29) = happyShift action_24
action_20 (32) = happyShift action_25
action_20 (33) = happyShift action_26
action_20 (36) = happyShift action_27
action_20 (5) = happyGoto action_37
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (17) = happyShift action_6
action_21 (20) = happyShift action_7
action_21 (31) = happyShift action_8
action_21 (7) = happyGoto action_36
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (17) = happyShift action_6
action_22 (20) = happyShift action_7
action_22 (31) = happyShift action_8
action_22 (7) = happyGoto action_35
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (20) = happyShift action_33
action_23 (8) = happyGoto action_34
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (20) = happyShift action_33
action_24 (8) = happyGoto action_32
action_24 _ = happyFail (happyExpListPerState 24)

action_25 _ = happyReduce_13

action_26 _ = happyReduce_14

action_27 _ = happyReduce_10

action_28 _ = happyReduce_2

action_29 (13) = happyShift action_16
action_29 (14) = happyShift action_17
action_29 (17) = happyShift action_18
action_29 (18) = happyShift action_19
action_29 (20) = happyShift action_20
action_29 (25) = happyShift action_21
action_29 (26) = happyShift action_22
action_29 (28) = happyShift action_23
action_29 (29) = happyShift action_24
action_29 (32) = happyShift action_25
action_29 (33) = happyShift action_26
action_29 (36) = happyShift action_27
action_29 (5) = happyGoto action_31
action_29 _ = happyFail (happyExpListPerState 29)

action_30 _ = happyReduce_26

action_31 (13) = happyShift action_16
action_31 (14) = happyShift action_17
action_31 (16) = happyShift action_42
action_31 (17) = happyShift action_18
action_31 (18) = happyShift action_19
action_31 (19) = happyShift action_62
action_31 (20) = happyShift action_20
action_31 (22) = happyShift action_44
action_31 (25) = happyShift action_21
action_31 (26) = happyShift action_22
action_31 (27) = happyShift action_45
action_31 (28) = happyShift action_23
action_31 (29) = happyShift action_24
action_31 (32) = happyShift action_25
action_31 (33) = happyShift action_26
action_31 (35) = happyShift action_46
action_31 (36) = happyShift action_27
action_31 (5) = happyGoto action_41
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_17

action_33 (18) = happyShift action_61
action_33 (9) = happyGoto action_58
action_33 (10) = happyGoto action_59
action_33 (11) = happyGoto action_60
action_33 _ = happyReduce_28

action_34 _ = happyReduce_16

action_35 (23) = happyShift action_56
action_35 (27) = happyShift action_13
action_35 (6) = happyGoto action_57
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (23) = happyShift action_56
action_36 (27) = happyShift action_13
action_36 (6) = happyGoto action_55
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (13) = happyShift action_16
action_37 (14) = happyShift action_17
action_37 (16) = happyShift action_42
action_37 (17) = happyShift action_18
action_37 (18) = happyShift action_19
action_37 (20) = happyShift action_20
action_37 (21) = happyShift action_54
action_37 (22) = happyShift action_44
action_37 (25) = happyShift action_21
action_37 (26) = happyShift action_22
action_37 (27) = happyShift action_45
action_37 (28) = happyShift action_23
action_37 (29) = happyShift action_24
action_37 (32) = happyShift action_25
action_37 (33) = happyShift action_26
action_37 (35) = happyShift action_46
action_37 (36) = happyShift action_27
action_37 (5) = happyGoto action_41
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (5) = happyGoto action_41
action_38 _ = happyReduce_7

action_39 (13) = happyFail []
action_39 (14) = happyFail []
action_39 (17) = happyFail []
action_39 (18) = happyFail []
action_39 (20) = happyShift action_20
action_39 (25) = happyFail []
action_39 (26) = happyFail []
action_39 (27) = happyShift action_45
action_39 (28) = happyFail []
action_39 (29) = happyFail []
action_39 (32) = happyFail []
action_39 (33) = happyFail []
action_39 (35) = happyShift action_46
action_39 (36) = happyFail []
action_39 (5) = happyGoto action_41
action_39 _ = happyReduce_21

action_40 (22) = happyShift action_53
action_40 (27) = happyShift action_13
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (5) = happyGoto action_41
action_41 _ = happyReduce_5

action_42 (13) = happyShift action_16
action_42 (14) = happyShift action_17
action_42 (17) = happyShift action_18
action_42 (18) = happyShift action_19
action_42 (20) = happyShift action_20
action_42 (25) = happyShift action_21
action_42 (26) = happyShift action_22
action_42 (28) = happyShift action_23
action_42 (29) = happyShift action_24
action_42 (32) = happyShift action_25
action_42 (33) = happyShift action_26
action_42 (36) = happyShift action_27
action_42 (5) = happyGoto action_52
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (13) = happyShift action_16
action_43 (14) = happyShift action_17
action_43 (17) = happyShift action_18
action_43 (18) = happyShift action_19
action_43 (20) = happyShift action_20
action_43 (25) = happyShift action_21
action_43 (26) = happyShift action_22
action_43 (28) = happyShift action_23
action_43 (29) = happyShift action_24
action_43 (32) = happyShift action_25
action_43 (33) = happyShift action_26
action_43 (36) = happyShift action_27
action_43 (5) = happyGoto action_51
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (33) = happyShift action_49
action_44 (34) = happyShift action_50
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (13) = happyShift action_16
action_45 (14) = happyShift action_17
action_45 (17) = happyShift action_18
action_45 (18) = happyShift action_19
action_45 (20) = happyShift action_20
action_45 (25) = happyShift action_21
action_45 (26) = happyShift action_22
action_45 (28) = happyShift action_23
action_45 (29) = happyShift action_24
action_45 (32) = happyShift action_25
action_45 (33) = happyShift action_26
action_45 (36) = happyShift action_27
action_45 (5) = happyGoto action_48
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (13) = happyShift action_16
action_46 (14) = happyShift action_17
action_46 (17) = happyShift action_18
action_46 (18) = happyShift action_19
action_46 (20) = happyShift action_20
action_46 (25) = happyShift action_21
action_46 (26) = happyShift action_22
action_46 (28) = happyShift action_23
action_46 (29) = happyShift action_24
action_46 (32) = happyShift action_25
action_46 (33) = happyShift action_26
action_46 (36) = happyShift action_27
action_46 (5) = happyGoto action_47
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (20) = happyShift action_20
action_47 (5) = happyGoto action_41
action_47 _ = happyReduce_19

action_48 (20) = happyShift action_20
action_48 (5) = happyGoto action_41
action_48 _ = happyReduce_15

action_49 _ = happyReduce_11

action_50 _ = happyReduce_12

action_51 (13) = happyShift action_16
action_51 (14) = happyShift action_17
action_51 (16) = happyShift action_42
action_51 (17) = happyShift action_18
action_51 (18) = happyShift action_19
action_51 (20) = happyShift action_20
action_51 (22) = happyShift action_44
action_51 (25) = happyShift action_21
action_51 (26) = happyShift action_22
action_51 (27) = happyShift action_45
action_51 (28) = happyShift action_23
action_51 (29) = happyShift action_24
action_51 (32) = happyShift action_25
action_51 (33) = happyShift action_26
action_51 (35) = happyShift action_46
action_51 (36) = happyShift action_27
action_51 (5) = happyGoto action_41
action_51 _ = happyReduce_39

action_52 (13) = happyShift action_16
action_52 (14) = happyShift action_17
action_52 (16) = happyShift action_42
action_52 (17) = happyShift action_18
action_52 (18) = happyShift action_19
action_52 (20) = happyShift action_20
action_52 (25) = happyShift action_21
action_52 (26) = happyShift action_22
action_52 (27) = happyShift action_45
action_52 (28) = happyShift action_23
action_52 (29) = happyShift action_24
action_52 (32) = happyShift action_25
action_52 (33) = happyShift action_26
action_52 (35) = happyShift action_46
action_52 (36) = happyShift action_27
action_52 (5) = happyGoto action_41
action_52 _ = happyReduce_18

action_53 (13) = happyShift action_16
action_53 (14) = happyShift action_17
action_53 (17) = happyShift action_18
action_53 (18) = happyShift action_19
action_53 (20) = happyShift action_20
action_53 (25) = happyShift action_21
action_53 (26) = happyShift action_22
action_53 (28) = happyShift action_23
action_53 (29) = happyShift action_24
action_53 (32) = happyShift action_25
action_53 (33) = happyShift action_26
action_53 (36) = happyShift action_27
action_53 (5) = happyGoto action_74
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_20

action_55 (13) = happyShift action_16
action_55 (14) = happyShift action_17
action_55 (17) = happyShift action_18
action_55 (18) = happyShift action_19
action_55 (20) = happyShift action_20
action_55 (25) = happyShift action_21
action_55 (26) = happyShift action_22
action_55 (28) = happyShift action_23
action_55 (29) = happyShift action_24
action_55 (32) = happyShift action_25
action_55 (33) = happyShift action_26
action_55 (36) = happyShift action_27
action_55 (5) = happyGoto action_73
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (13) = happyShift action_16
action_56 (14) = happyShift action_17
action_56 (17) = happyShift action_18
action_56 (18) = happyShift action_19
action_56 (20) = happyShift action_20
action_56 (25) = happyShift action_21
action_56 (26) = happyShift action_22
action_56 (28) = happyShift action_23
action_56 (29) = happyShift action_24
action_56 (32) = happyShift action_25
action_56 (33) = happyShift action_26
action_56 (36) = happyShift action_27
action_56 (5) = happyGoto action_72
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (13) = happyShift action_16
action_57 (14) = happyShift action_17
action_57 (17) = happyShift action_18
action_57 (18) = happyShift action_19
action_57 (20) = happyShift action_20
action_57 (25) = happyShift action_21
action_57 (26) = happyShift action_22
action_57 (28) = happyShift action_23
action_57 (29) = happyShift action_24
action_57 (32) = happyShift action_25
action_57 (33) = happyShift action_26
action_57 (36) = happyShift action_27
action_57 (5) = happyGoto action_71
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (21) = happyShift action_70
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_29

action_60 _ = happyReduce_30

action_61 (13) = happyShift action_16
action_61 (14) = happyShift action_17
action_61 (16) = happyShift action_66
action_61 (17) = happyShift action_67
action_61 (18) = happyShift action_19
action_61 (20) = happyShift action_68
action_61 (25) = happyShift action_21
action_61 (26) = happyShift action_22
action_61 (28) = happyShift action_23
action_61 (29) = happyShift action_24
action_61 (30) = happyShift action_69
action_61 (31) = happyShift action_8
action_61 (32) = happyShift action_25
action_61 (33) = happyShift action_26
action_61 (36) = happyShift action_27
action_61 (5) = happyGoto action_64
action_61 (7) = happyGoto action_65
action_61 _ = happyReduce_32

action_62 (13) = happyShift action_16
action_62 (14) = happyShift action_17
action_62 (17) = happyShift action_18
action_62 (18) = happyShift action_19
action_62 (20) = happyShift action_20
action_62 (25) = happyShift action_21
action_62 (26) = happyShift action_22
action_62 (28) = happyShift action_23
action_62 (29) = happyShift action_24
action_62 (32) = happyShift action_25
action_62 (33) = happyShift action_26
action_62 (36) = happyShift action_27
action_62 (5) = happyGoto action_63
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (13) = happyShift action_16
action_63 (14) = happyShift action_17
action_63 (16) = happyShift action_42
action_63 (17) = happyShift action_18
action_63 (18) = happyShift action_19
action_63 (20) = happyShift action_20
action_63 (22) = happyShift action_44
action_63 (25) = happyShift action_21
action_63 (26) = happyShift action_22
action_63 (27) = happyShift action_45
action_63 (28) = happyShift action_23
action_63 (29) = happyShift action_24
action_63 (32) = happyShift action_25
action_63 (33) = happyShift action_26
action_63 (35) = happyShift action_46
action_63 (36) = happyShift action_27
action_63 (5) = happyGoto action_41
action_63 _ = happyReduce_40

action_64 (13) = happyShift action_16
action_64 (14) = happyShift action_17
action_64 (16) = happyShift action_42
action_64 (17) = happyShift action_18
action_64 (18) = happyShift action_19
action_64 (20) = happyShift action_20
action_64 (22) = happyShift action_44
action_64 (25) = happyShift action_21
action_64 (26) = happyShift action_22
action_64 (27) = happyShift action_45
action_64 (28) = happyShift action_23
action_64 (29) = happyShift action_24
action_64 (30) = happyShift action_80
action_64 (32) = happyShift action_25
action_64 (33) = happyShift action_26
action_64 (35) = happyShift action_46
action_64 (36) = happyShift action_27
action_64 (5) = happyGoto action_41
action_64 _ = happyReduce_31

action_65 (16) = happyShift action_79
action_65 (27) = happyShift action_13
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (13) = happyShift action_16
action_66 (14) = happyShift action_17
action_66 (17) = happyShift action_18
action_66 (18) = happyShift action_19
action_66 (20) = happyShift action_20
action_66 (25) = happyShift action_21
action_66 (26) = happyShift action_22
action_66 (28) = happyShift action_23
action_66 (29) = happyShift action_24
action_66 (32) = happyShift action_25
action_66 (33) = happyShift action_26
action_66 (36) = happyShift action_27
action_66 (5) = happyGoto action_78
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (16) = happyReduce_4
action_67 (21) = happyReduce_4
action_67 (27) = happyReduce_4
action_67 _ = happyReduce_4

action_68 (13) = happyShift action_16
action_68 (14) = happyShift action_17
action_68 (17) = happyShift action_67
action_68 (18) = happyShift action_19
action_68 (20) = happyShift action_68
action_68 (25) = happyShift action_21
action_68 (26) = happyShift action_22
action_68 (28) = happyShift action_23
action_68 (29) = happyShift action_24
action_68 (31) = happyShift action_8
action_68 (32) = happyShift action_25
action_68 (33) = happyShift action_26
action_68 (36) = happyShift action_27
action_68 (5) = happyGoto action_37
action_68 (7) = happyGoto action_9
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (18) = happyShift action_77
action_69 (10) = happyGoto action_76
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_27

action_71 (13) = happyShift action_16
action_71 (14) = happyShift action_17
action_71 (16) = happyShift action_42
action_71 (17) = happyShift action_18
action_71 (18) = happyShift action_19
action_71 (20) = happyShift action_20
action_71 (22) = happyShift action_44
action_71 (25) = happyShift action_21
action_71 (26) = happyShift action_22
action_71 (27) = happyShift action_45
action_71 (28) = happyShift action_23
action_71 (29) = happyShift action_24
action_71 (32) = happyShift action_25
action_71 (33) = happyShift action_26
action_71 (35) = happyShift action_46
action_71 (36) = happyShift action_27
action_71 (5) = happyGoto action_41
action_71 _ = happyReduce_9

action_72 (13) = happyShift action_16
action_72 (14) = happyShift action_17
action_72 (16) = happyShift action_42
action_72 (17) = happyShift action_18
action_72 (18) = happyShift action_19
action_72 (20) = happyShift action_20
action_72 (22) = happyShift action_75
action_72 (25) = happyShift action_21
action_72 (26) = happyShift action_22
action_72 (27) = happyShift action_45
action_72 (28) = happyShift action_23
action_72 (29) = happyShift action_24
action_72 (32) = happyShift action_25
action_72 (33) = happyShift action_26
action_72 (35) = happyShift action_46
action_72 (36) = happyShift action_27
action_72 (5) = happyGoto action_41
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (13) = happyShift action_16
action_73 (14) = happyShift action_17
action_73 (16) = happyShift action_42
action_73 (17) = happyShift action_18
action_73 (18) = happyShift action_19
action_73 (20) = happyShift action_20
action_73 (22) = happyShift action_44
action_73 (25) = happyShift action_21
action_73 (26) = happyShift action_22
action_73 (27) = happyShift action_45
action_73 (28) = happyShift action_23
action_73 (29) = happyShift action_24
action_73 (32) = happyShift action_25
action_73 (33) = happyShift action_26
action_73 (35) = happyShift action_46
action_73 (36) = happyShift action_27
action_73 (5) = happyGoto action_41
action_73 _ = happyReduce_8

action_74 (13) = happyShift action_16
action_74 (14) = happyShift action_17
action_74 (16) = happyShift action_42
action_74 (17) = happyShift action_18
action_74 (18) = happyShift action_19
action_74 (20) = happyShift action_20
action_74 (25) = happyShift action_21
action_74 (26) = happyShift action_22
action_74 (27) = happyShift action_45
action_74 (28) = happyShift action_23
action_74 (29) = happyShift action_24
action_74 (32) = happyShift action_25
action_74 (33) = happyShift action_26
action_74 (35) = happyShift action_46
action_74 (36) = happyShift action_27
action_74 (5) = happyGoto action_41
action_74 _ = happyReduce_3

action_75 (33) = happyShift action_49
action_75 (34) = happyShift action_50
action_75 _ = happyReduce_22

action_76 _ = happyReduce_34

action_77 (13) = happyShift action_16
action_77 (14) = happyShift action_17
action_77 (17) = happyShift action_18
action_77 (18) = happyShift action_19
action_77 (20) = happyShift action_20
action_77 (25) = happyShift action_21
action_77 (26) = happyShift action_22
action_77 (28) = happyShift action_23
action_77 (29) = happyShift action_24
action_77 (30) = happyShift action_69
action_77 (32) = happyShift action_25
action_77 (33) = happyShift action_26
action_77 (36) = happyShift action_27
action_77 (5) = happyGoto action_64
action_77 _ = happyReduce_32

action_78 (13) = happyShift action_16
action_78 (14) = happyShift action_17
action_78 (16) = happyShift action_42
action_78 (17) = happyShift action_18
action_78 (18) = happyShift action_19
action_78 (20) = happyShift action_20
action_78 (22) = happyShift action_44
action_78 (25) = happyShift action_21
action_78 (26) = happyShift action_22
action_78 (27) = happyShift action_45
action_78 (28) = happyShift action_23
action_78 (29) = happyShift action_24
action_78 (30) = happyShift action_83
action_78 (32) = happyShift action_25
action_78 (33) = happyShift action_26
action_78 (35) = happyShift action_46
action_78 (36) = happyShift action_27
action_78 (5) = happyGoto action_41
action_78 _ = happyReduce_36

action_79 (13) = happyShift action_16
action_79 (14) = happyShift action_17
action_79 (17) = happyShift action_18
action_79 (18) = happyShift action_19
action_79 (20) = happyShift action_20
action_79 (25) = happyShift action_21
action_79 (26) = happyShift action_22
action_79 (28) = happyShift action_23
action_79 (29) = happyShift action_24
action_79 (32) = happyShift action_25
action_79 (33) = happyShift action_26
action_79 (36) = happyShift action_27
action_79 (5) = happyGoto action_82
action_79 _ = happyFail (happyExpListPerState 79)

action_80 (18) = happyShift action_77
action_80 (10) = happyGoto action_81
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_33

action_82 (13) = happyShift action_16
action_82 (14) = happyShift action_17
action_82 (16) = happyShift action_42
action_82 (17) = happyShift action_18
action_82 (18) = happyShift action_19
action_82 (20) = happyShift action_20
action_82 (22) = happyShift action_44
action_82 (25) = happyShift action_21
action_82 (26) = happyShift action_22
action_82 (27) = happyShift action_45
action_82 (28) = happyShift action_23
action_82 (29) = happyShift action_24
action_82 (30) = happyShift action_86
action_82 (32) = happyShift action_25
action_82 (33) = happyShift action_26
action_82 (35) = happyShift action_46
action_82 (36) = happyShift action_27
action_82 (5) = happyGoto action_41
action_82 _ = happyReduce_35

action_83 (18) = happyShift action_85
action_83 (11) = happyGoto action_84
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_38

action_85 (16) = happyShift action_66
action_85 (17) = happyShift action_6
action_85 (20) = happyShift action_7
action_85 (31) = happyShift action_8
action_85 (7) = happyGoto action_65
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (18) = happyShift action_85
action_86 (11) = happyGoto action_87
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_37

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 (ExprZero
	)

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn4
		 (ExprDecl happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happyReduce 4 5 happyReduction_3
happyReduction_3 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ExprLam happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn5
		 (ExprName happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ExprApp happy_var_1 happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyTerminal (Token _ (TokenConstr happy_var_1)))
	 =  HappyAbsSyn5
		 (ExprConstr happy_var_1 ExprZero
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (Token _ (TokenConstr happy_var_1)))
	 =  HappyAbsSyn5
		 (ExprConstr happy_var_1 happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 4 5 happyReduction_8
happyReduction_8 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ExprPi happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 5 happyReduction_9
happyReduction_9 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ExprSigma happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_10 = happySpecReduce_1  5 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn5
		 (ExprU
	)

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 _
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ExprPrj1 happy_var_1
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 _
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ExprPrj2 happy_var_1
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  5 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn5
		 (ExprZero
	)

happyReduce_14 = happySpecReduce_1  5 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn5
		 (ExprUnit
	)

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ExprProduct happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  5 happyReduction_16
happyReduction_16 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (ExprCaseFun happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  5 happyReduction_17
happyReduction_17 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (ExprSum happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  5 happyReduction_18
happyReduction_18 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ExprPi PatDummy happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  5 happyReduction_19
happyReduction_19 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (ExprSigma PatDummy happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  5 happyReduction_20
happyReduction_20 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  5 happyReduction_21
happyReduction_21 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (ExprRecUnit happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  6 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  7 happyReduction_23
happyReduction_23 (HappyTerminal (Token _ (TokenVar happy_var_1)))
	 =  HappyAbsSyn7
		 (PatName happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  7 happyReduction_24
happyReduction_24 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (PatProduct happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  7 happyReduction_25
happyReduction_25 _
	 =  HappyAbsSyn7
		 (PatDummy
	)

happyReduce_26 = happySpecReduce_3  7 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  8 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_0  9 happyReduction_28
happyReduction_28  =  HappyAbsSyn9
		 ([]
	)

happyReduce_29 = happySpecReduce_1  9 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  9 happyReduction_30
happyReduction_30 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_2  10 happyReduction_31
happyReduction_31 (HappyAbsSyn5  happy_var_2)
	(HappyTerminal (Token _ (TokenConstr happy_var_1)))
	 =  HappyAbsSyn10
		 ([ (happy_var_1 , happy_var_2) ]
	)
happyReduction_31 _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  10 happyReduction_32
happyReduction_32 (HappyTerminal (Token _ (TokenConstr happy_var_1)))
	 =  HappyAbsSyn10
		 ([ (happy_var_1 , ExprUnit) ]
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happyReduce 4 10 happyReduction_33
happyReduction_33 ((HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	(HappyTerminal (Token _ (TokenConstr happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ((happy_var_1 , happy_var_2) : happy_var_4
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_3  10 happyReduction_34
happyReduction_34 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (Token _ (TokenConstr happy_var_1)))
	 =  HappyAbsSyn10
		 ((happy_var_1 , ExprUnit) : happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happyReduce 4 11 happyReduction_35
happyReduction_35 ((HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyTerminal (Token _ (TokenConstr happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ([ (happy_var_1 , ExprLam happy_var_2 happy_var_4) ]
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_3  11 happyReduction_36
happyReduction_36 (HappyAbsSyn5  happy_var_3)
	_
	(HappyTerminal (Token _ (TokenConstr happy_var_1)))
	 =  HappyAbsSyn11
		 ([ (happy_var_1 , ExprRecUnit happy_var_3) ]
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 6 11 happyReduction_37
happyReduction_37 ((HappyAbsSyn11  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	(HappyTerminal (Token _ (TokenConstr happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((happy_var_1 , ExprLam happy_var_2 happy_var_4) : happy_var_6
	) `HappyStk` happyRest

happyReduce_38 = happyReduce 5 11 happyReduction_38
happyReduction_38 ((HappyAbsSyn11  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Token _ (TokenConstr happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 ((happy_var_1 , ExprRecUnit happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_39 = happyReduce 5 12 happyReduction_39
happyReduction_39 ((HappyAbsSyn5  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (DeclRegular happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 6 12 happyReduction_40
happyReduction_40 ((HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (DeclRec happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyNewToken action sts stk
	= lexwrap(\tk -> 
	let cont i = action i i tk (HappyState action) sts stk in
	case tk of {
	Token _ TokenEOF -> action 37 37 tk (HappyState action) sts stk;
	Token _ TokenLam -> cont 13;
	Token _ TokenRecUnit -> cont 14;
	Token _ TokenRec -> cont 15;
	Token _ TokenArrow -> cont 16;
	Token _ (TokenVar happy_dollar_dollar) -> cont 17;
	Token _ (TokenConstr happy_dollar_dollar) -> cont 18;
	Token _ TokenEq -> cont 19;
	Token _ TokenLParen -> cont 20;
	Token _ TokenRParen -> cont 21;
	Token _ TokenDot -> cont 22;
	Token _ TokenColon -> cont 23;
	Token _ TokenSemiColon -> cont 24;
	Token _ TokenPi -> cont 25;
	Token _ TokenSigma -> cont 26;
	Token _ TokenComma -> cont 27;
	Token _ TokenFun -> cont 28;
	Token _ TokenSum -> cont 29;
	Token _ TokenVBar -> cont 30;
	Token _ TokenDummy -> cont 31;
	Token _ TokenZero -> cont 32;
	Token _ TokenOne -> cont 33;
	Token _ TokenTwo -> cont 34;
	Token _ TokenTimes -> cont 35;
	Token _ TokenU -> cont 36;
	_ -> happyError' (tk, [])
	})

happyError_ explist 37 tk = happyError' (tk, explist)
happyError_ explist _ tk = happyError' (tk, explist)

happyThen :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen = (>>=)
happyReturn :: () => a -> Alex a
happyReturn = (return)
happyThen1 :: () => Alex a -> (a -> Alex b) -> Alex b
happyThen1 = happyThen
happyReturn1 :: () => a -> Alex a
happyReturn1 = happyReturn
happyError' :: () => ((Token), [String]) -> Alex a
happyError' tk = (\(tokens, _) -> happyError tokens) tk
parse = happySomeParser where
 happySomeParser = happyThen (happyParse action_0) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) =
    alexError' p ("parse error at token '" ++ show t ++ "'")

parseExpr:: FilePath -> String -> Either String Expr
parseExpr = runAlex' parse
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 15 "<built-in>" #-}
{-# LINE 1 "/Users/Komma/.stack/programs/x86_64-osx/ghc-8.2.2/lib/ghc-8.2.2/include/ghcversion.h" #-}
















{-# LINE 16 "<built-in>" #-}
{-# LINE 1 "/var/folders/dm/xldttyq55vgc95z5wf3w17_c0000gn/T/ghc12646_0/ghc_2.h" #-}



































































































































































{-# LINE 17 "<built-in>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 










{-# LINE 43 "templates/GenericTemplate.hs" #-}

data Happy_IntList = HappyCons Int Happy_IntList








{-# LINE 65 "templates/GenericTemplate.hs" #-}


{-# LINE 75 "templates/GenericTemplate.hs" #-}










infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action


{-# LINE 137 "templates/GenericTemplate.hs" #-}


{-# LINE 147 "templates/GenericTemplate.hs" #-}
indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
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
     = case happyDrop (k - ((1) :: Int)) sts of
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





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
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

