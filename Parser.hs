{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import Syntax
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15
	| HappyAbsSyn16 t16
	| HappyAbsSyn17 t17
	| HappyAbsSyn18 t18
	| HappyAbsSyn19 t19
	| HappyAbsSyn20 t20

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,187) ([0,1072,5,0,2048,0,0,0,0,0,0,0,0,260,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,256,0,0,0,8192,0,0,64,16,0,256,0,0,0,4096,0,0,8192,0,0,0,0,1024,0,32768,0,0,1024,0,0,0,0,0,0,256,0,0,0,8192,0,0,64,0,0,0,0,8,0,0,0,4,0,32768,0,0,0,0,1,0,8192,0,0,0,16,0,0,0,32,0,0,8,0,0,4096,0,0,0,1,0,8192,128,2,0,15880,128,0,24,0,0,0,4100,64,0,512,8200,0,0,0,0,0,0,0,0,0,0,4,0,0,0,0,0,2,0,0,0,0,0,16384,0,0,16384,0,0,32768,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,16400,256,0,2048,32800,0,0,4096,0,0,0,8192,0,0,0,112,0,0,0,0,0,512,0,24576,0,1,0,4096,64,1,0,0,0,0,1024,16400,0,0,2050,32,0,0,28672,0,0,0,56,0,0,7168,0,0,0,14,0,0,1792,0,0,32768,3,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,768,0,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,16384,0,0,32768,1,0,0,192,0,0,0,0,4,0,0,4096,0,0,0,4,0,0,16,0,0,0,0,0,0,1024,0,0,512,0,0,0,1,0,0,1024,0,0,0,8,0,0,64,0,0,0,0,0,0,8,0,0,0,2,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Query","SelectFile","SelectQuery","CreateEdgeQuery","UpdateQuery","DeleteQuery","DeleteEdgeQuery","PropertyRefs","PropertyRef","NodePatterns","NodePattern","EdgeType","EdgePattern","Condition","PropertyUpdates","PropertyUpdate","Expression","SELECT","FROM","WHERE","AND","OR","LIMIT","CREATE","EDGE","TO","TYPE","STARTS_WITH","NOT","UPDATE","SET","DELETE","','","'='","'<'","'>'","'<='","'>='","'('","')'","'.'","':'","'*'","'->'","'['","']'","'-'","HAS","IDENT","STRING","INT","%eof"]
        bit_start = st Prelude.* 55
        bit_end = (st Prelude.+ 1) Prelude.* 55
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..54]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (21) = happyShift action_3
action_0 (22) = happyShift action_10
action_0 (27) = happyShift action_11
action_0 (33) = happyShift action_12
action_0 (35) = happyShift action_13
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_5
action_0 (6) = happyGoto action_2
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (9) = happyGoto action_8
action_0 (10) = happyGoto action_9
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (21) = happyShift action_3
action_1 (6) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (46) = happyShift action_23
action_3 (52) = happyShift action_24
action_3 (11) = happyGoto action_21
action_3 (12) = happyGoto action_22
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (55) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_6

action_6 _ = happyReduce_2

action_7 _ = happyReduce_3

action_8 _ = happyReduce_4

action_9 _ = happyReduce_5

action_10 (53) = happyShift action_20
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (28) = happyShift action_19
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (42) = happyShift action_17
action_12 (13) = happyGoto action_18
action_12 (14) = happyGoto action_15
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (28) = happyShift action_16
action_13 (42) = happyShift action_17
action_13 (13) = happyGoto action_14
action_13 (14) = happyGoto action_15
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (23) = happyShift action_34
action_14 _ = happyReduce_15

action_15 (36) = happyShift action_33
action_15 _ = happyReduce_20

action_16 (30) = happyShift action_32
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (52) = happyShift action_31
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (34) = happyShift action_30
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (22) = happyShift action_29
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_7

action_21 (22) = happyShift action_28
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (36) = happyShift action_27
action_22 _ = happyReduce_17

action_23 (22) = happyShift action_26
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (44) = happyShift action_25
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (52) = happyShift action_50
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (42) = happyShift action_17
action_26 (13) = happyGoto action_49
action_26 (14) = happyGoto action_15
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (52) = happyShift action_24
action_27 (11) = happyGoto action_48
action_27 (12) = happyGoto action_22
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (42) = happyShift action_17
action_28 (13) = happyGoto action_47
action_28 (14) = happyGoto action_15
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (42) = happyShift action_17
action_29 (14) = happyGoto action_46
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (52) = happyShift action_24
action_30 (12) = happyGoto action_43
action_30 (18) = happyGoto action_44
action_30 (19) = happyGoto action_45
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (43) = happyShift action_42
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (45) = happyShift action_41
action_32 (15) = happyGoto action_40
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (42) = happyShift action_17
action_33 (13) = happyGoto action_39
action_33 (14) = happyGoto action_15
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (32) = happyShift action_37
action_34 (42) = happyShift action_38
action_34 (52) = happyShift action_24
action_34 (12) = happyGoto action_35
action_34 (17) = happyGoto action_36
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (31) = happyShift action_63
action_35 (37) = happyShift action_64
action_35 (38) = happyShift action_65
action_35 (39) = happyShift action_66
action_35 (40) = happyShift action_67
action_35 (41) = happyShift action_68
action_35 (51) = happyShift action_69
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (24) = happyShift action_61
action_36 (25) = happyShift action_62
action_36 _ = happyReduce_14

action_37 (32) = happyShift action_37
action_37 (42) = happyShift action_38
action_37 (52) = happyShift action_24
action_37 (12) = happyGoto action_35
action_37 (17) = happyGoto action_60
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (32) = happyShift action_37
action_38 (42) = happyShift action_59
action_38 (52) = happyShift action_24
action_38 (12) = happyGoto action_35
action_38 (14) = happyGoto action_57
action_38 (17) = happyGoto action_58
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_21

action_40 _ = happyReduce_16

action_41 (52) = happyShift action_56
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_22

action_43 (37) = happyShift action_55
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_13

action_45 (36) = happyShift action_54
action_45 _ = happyReduce_37

action_46 (29) = happyShift action_53
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (23) = happyShift action_52
action_47 _ = happyReduce_9

action_48 _ = happyReduce_18

action_49 (23) = happyShift action_51
action_49 _ = happyReduce_11

action_50 _ = happyReduce_19

action_51 (32) = happyShift action_37
action_51 (42) = happyShift action_38
action_51 (52) = happyShift action_24
action_51 (12) = happyGoto action_35
action_51 (17) = happyGoto action_89
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (32) = happyShift action_37
action_52 (42) = happyShift action_38
action_52 (52) = happyShift action_24
action_52 (12) = happyGoto action_35
action_52 (17) = happyGoto action_88
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (42) = happyShift action_17
action_53 (14) = happyGoto action_87
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (52) = happyShift action_24
action_54 (12) = happyGoto action_43
action_54 (18) = happyGoto action_86
action_54 (19) = happyGoto action_45
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (52) = happyShift action_24
action_55 (53) = happyShift action_73
action_55 (54) = happyShift action_74
action_55 (12) = happyGoto action_71
action_55 (20) = happyGoto action_85
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_23

action_57 (43) = happyShift action_84
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (24) = happyShift action_61
action_58 (25) = happyShift action_62
action_58 (43) = happyShift action_83
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (32) = happyShift action_37
action_59 (42) = happyShift action_59
action_59 (52) = happyShift action_82
action_59 (12) = happyGoto action_35
action_59 (14) = happyGoto action_57
action_59 (17) = happyGoto action_58
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (24) = happyShift action_61
action_60 (25) = happyShift action_62
action_60 _ = happyReduce_27

action_61 (32) = happyShift action_37
action_61 (42) = happyShift action_38
action_61 (52) = happyShift action_24
action_61 (12) = happyGoto action_35
action_61 (17) = happyGoto action_81
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (32) = happyShift action_37
action_62 (42) = happyShift action_38
action_62 (52) = happyShift action_24
action_62 (12) = happyGoto action_35
action_62 (17) = happyGoto action_80
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (52) = happyShift action_24
action_63 (53) = happyShift action_73
action_63 (54) = happyShift action_74
action_63 (12) = happyGoto action_71
action_63 (20) = happyGoto action_79
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (52) = happyShift action_24
action_64 (53) = happyShift action_73
action_64 (54) = happyShift action_74
action_64 (12) = happyGoto action_71
action_64 (20) = happyGoto action_78
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (52) = happyShift action_24
action_65 (53) = happyShift action_73
action_65 (54) = happyShift action_74
action_65 (12) = happyGoto action_71
action_65 (20) = happyGoto action_77
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (52) = happyShift action_24
action_66 (53) = happyShift action_73
action_66 (54) = happyShift action_74
action_66 (12) = happyGoto action_71
action_66 (20) = happyGoto action_76
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (52) = happyShift action_24
action_67 (53) = happyShift action_73
action_67 (54) = happyShift action_74
action_67 (12) = happyGoto action_71
action_67 (20) = happyGoto action_75
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (52) = happyShift action_24
action_68 (53) = happyShift action_73
action_68 (54) = happyShift action_74
action_68 (12) = happyGoto action_71
action_68 (20) = happyGoto action_72
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (53) = happyShift action_70
action_69 _ = happyFail (happyExpListPerState 69)

action_70 _ = happyReduce_34

action_71 _ = happyReduce_42

action_72 _ = happyReduce_32

action_73 _ = happyReduce_41

action_74 _ = happyReduce_40

action_75 _ = happyReduce_31

action_76 _ = happyReduce_30

action_77 _ = happyReduce_29

action_78 _ = happyReduce_28

action_79 _ = happyReduce_33

action_80 (24) = happyShift action_61
action_80 (25) = happyShift action_62
action_80 _ = happyReduce_26

action_81 (24) = happyShift action_61
action_81 (25) = happyShift action_62
action_81 _ = happyReduce_25

action_82 (43) = happyShift action_42
action_82 (44) = happyShift action_25
action_82 _ = happyFail (happyExpListPerState 82)

action_83 _ = happyReduce_36

action_84 (50) = happyShift action_91
action_84 _ = happyFail (happyExpListPerState 84)

action_85 _ = happyReduce_39

action_86 _ = happyReduce_38

action_87 (30) = happyShift action_90
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (24) = happyShift action_61
action_88 (25) = happyShift action_62
action_88 _ = happyReduce_8

action_89 (24) = happyShift action_61
action_89 (25) = happyShift action_62
action_89 _ = happyReduce_10

action_90 (45) = happyShift action_41
action_90 (15) = happyGoto action_94
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (48) = happyShift action_93
action_91 (16) = happyGoto action_92
action_91 _ = happyFail (happyExpListPerState 91)

action_92 (47) = happyShift action_96
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (42) = happyShift action_17
action_93 (14) = happyGoto action_95
action_93 _ = happyFail (happyExpListPerState 93)

action_94 _ = happyReduce_12

action_95 (50) = happyShift action_98
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (42) = happyShift action_97
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (42) = happyShift action_17
action_97 (14) = happyGoto action_100
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (45) = happyShift action_41
action_98 (15) = happyGoto action_99
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (47) = happyShift action_102
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (43) = happyShift action_101
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_35

action_102 (42) = happyShift action_17
action_102 (14) = happyGoto action_103
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (49) = happyShift action_104
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_24

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  4 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_2  5 happyReduction_7
happyReduction_7 (HappyTerminal (Tok _ (TokString happy_var_2)))
	_
	 =  HappyAbsSyn5
		 (SelectFile happy_var_2
	)
happyReduction_7 _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 6 6 happyReduction_8
happyReduction_8 ((HappyAbsSyn17  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (SelectQuery happy_var_2 happy_var_4 (Just happy_var_6)
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 6 happyReduction_9
happyReduction_9 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (SelectQuery happy_var_2 happy_var_4 Nothing
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 6 6 happyReduction_10
happyReduction_10 ((HappyAbsSyn17  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (SelectQuery [PropertyRef "*" "*"] happy_var_4 (Just happy_var_6)
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 6 happyReduction_11
happyReduction_11 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (SelectQuery [PropertyRef "*" "*"] happy_var_4 Nothing
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 8 7 happyReduction_12
happyReduction_12 ((HappyAbsSyn15  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (CreateEdgeQuery happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 8 happyReduction_13
happyReduction_13 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (UpdateQuery happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 9 happyReduction_14
happyReduction_14 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (DeleteQuery happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_2  9 happyReduction_15
happyReduction_15 (HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (DeleteQuery happy_var_2 Nothing
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 10 happyReduction_16
happyReduction_16 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (DeleteEdgeQuery happy_var_4
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 (HappyTerminal (Tok _ (TokIdent happy_var_3)))
	_
	(HappyTerminal (Tok _ (TokIdent happy_var_1)))
	 =  HappyAbsSyn12
		 (PropertyRef happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  13 happyReduction_20
happyReduction_20 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  13 happyReduction_21
happyReduction_21 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  14 happyReduction_22
happyReduction_22 _
	(HappyTerminal (Tok _ (TokIdent happy_var_2)))
	_
	 =  HappyAbsSyn14
		 (NodePattern happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  15 happyReduction_23
happyReduction_23 (HappyTerminal (Tok _ (TokIdent happy_var_2)))
	_
	 =  HappyAbsSyn15
		 (EdgeType happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 7 16 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (EdgePattern happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_3  17 happyReduction_25
happyReduction_25 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (ConditionAnd happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  17 happyReduction_26
happyReduction_26 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn17  happy_var_1)
	 =  HappyAbsSyn17
		 (ConditionOr happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  17 happyReduction_27
happyReduction_27 (HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (ConditionNot happy_var_2
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  17 happyReduction_28
happyReduction_28 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn17
		 (ConditionEqual happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  17 happyReduction_29
happyReduction_29 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn17
		 (ConditionLess happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  17 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn17
		 (ConditionGreater happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  17 happyReduction_31
happyReduction_31 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn17
		 (ConditionLessEqual happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  17 happyReduction_32
happyReduction_32 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn17
		 (ConditionGreaterEqual happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  17 happyReduction_33
happyReduction_33 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn17
		 (ConditionStartsWith happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  17 happyReduction_34
happyReduction_34 (HappyTerminal (Tok _ (TokString happy_var_3)))
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn17
		 (ConditionHasLabel happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happyReduce 9 17 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn14  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (ConditionEdge happy_var_2 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_3  17 happyReduction_36
happyReduction_36 _
	(HappyAbsSyn17  happy_var_2)
	_
	 =  HappyAbsSyn17
		 (happy_var_2
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  18 happyReduction_37
happyReduction_37 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  18 happyReduction_38
happyReduction_38 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_3
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  19 happyReduction_39
happyReduction_39 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn19
		 (PropertyUpdate happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  20 happyReduction_40
happyReduction_40 (HappyTerminal (Tok _ (TokInt happy_var_1)))
	 =  HappyAbsSyn20
		 (ExpressionInteger happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  20 happyReduction_41
happyReduction_41 (HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn20
		 (ExpressionString happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  20 happyReduction_42
happyReduction_42 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn20
		 (ExpressionPropertyRef happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 55 55 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tok _ TokSelect -> cont 21;
	Tok _ TokFrom -> cont 22;
	Tok _ TokWhere -> cont 23;
	Tok _ TokAnd -> cont 24;
	Tok _ TokOr -> cont 25;
	Tok _ TokLimit -> cont 26;
	Tok _ TokCreate -> cont 27;
	Tok _ TokEdge -> cont 28;
	Tok _ TokTo -> cont 29;
	Tok _ TokType -> cont 30;
	Tok _ TokStartsWith -> cont 31;
	Tok _ TokNot -> cont 32;
	Tok _ TokUpdate -> cont 33;
	Tok _ TokSet -> cont 34;
	Tok _ TokDelete -> cont 35;
	Tok _ TokComma -> cont 36;
	Tok _ TokEqual -> cont 37;
	Tok _ TokLT -> cont 38;
	Tok _ TokGT -> cont 39;
	Tok _ TokLTE -> cont 40;
	Tok _ TokGTE -> cont 41;
	Tok _ TokLParen -> cont 42;
	Tok _ TokRParen -> cont 43;
	Tok _ TokPeriod -> cont 44;
	Tok _ TokColon -> cont 45;
	Tok _ TokAsterisk -> cont 46;
	Tok _ TokArrow -> cont 47;
	Tok _ TokLBracket -> cont 48;
	Tok _ TokRBracket -> cont 49;
	Tok _ TokMinus -> cont 50;
	Tok _ TokHas -> cont 51;
	Tok _ (TokIdent happy_dollar_dollar) -> cont 52;
	Tok _ (TokString happy_dollar_dollar) -> cont 53;
	Tok _ (TokInt happy_dollar_dollar) -> cont 54;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 55 tk tks = happyError' (tks, explist)
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
parseError tokens@(x:xs) = error $ "Parse error: " ++ tokenPosn (head tokens) ++ "\n" ++ show tokens
parseError [] = error "Parse error: end of input"
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
