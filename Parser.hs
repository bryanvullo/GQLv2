{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import Syntax
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19 t20 t21
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
	| HappyAbsSyn21 t21

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,199) ([0,2144,10,0,24576,2568,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4160,0,0,0,32,0,16,0,0,0,1024,0,0,16,4,0,0,0,0,24576,2568,0,0,0,0,0,32768,0,0,0,0,16,0,0,64,0,0,0,0,16,0,1024,0,0,64,0,0,0,0,0,0,64,0,0,0,4096,0,0,64,0,0,0,0,16,0,0,0,16,0,0,4,0,0,0,16,0,0,4,0,0,1024,0,0,0,4096,0,0,2048,0,0,0,32,0,0,1024,0,0,256,4100,0,32768,992,8,0,3,0,0,0,1025,16,0,256,4100,0,0,0,0,0,0,0,0,0,0,16,0,0,0,0,0,32,0,0,0,0,0,0,16,0,0,32,0,0,128,0,0,0,0,0,0,128,0,0,0,0,0,0,0,1025,16,0,256,4100,0,0,1024,0,0,0,4096,0,0,0,112,0,0,0,0,0,2048,0,0,3,8,0,0,1025,16,0,0,0,0,0,1025,16,0,256,4100,0,0,0,112,0,0,28672,0,0,0,112,0,0,28672,0,0,0,112,0,0,28672,0,0,0,32,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,16384,0,0,0,3,0,0,768,0,0,0,0,32,0,0,0,1,0,0,128,0,0,1024,0,0,0,0,0,0,0,4,0,0,4,0,0,1024,0,0,0,32,0,0,32768,0,0,0,8,0,0,0,0,0,0,4,0,0,0,2,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Queries","Query","SelectFile","SelectQuery","CreateEdgeQuery","UpdateQuery","DeleteQuery","DeleteEdgeQuery","PropertyRefs","PropertyRef","NodePatterns","NodePattern","EdgeType","EdgePattern","Condition","PropertyUpdates","PropertyUpdate","Expression","SELECT","FROM","WHERE","AND","OR","LIMIT","CREATE","EDGE","TO","TYPE","STARTS_WITH","NOT","UPDATE","SET","DELETE","','","'='","'<'","'>'","'<='","'>='","'('","')'","'.'","':'","'*'","'->'","'['","']'","'-'","HAS","IDENT","STRING","INT","%eof"]
        bit_start = st Prelude.* 56
        bit_end = (st Prelude.+ 1) Prelude.* 56
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..55]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (22) = happyShift action_9
action_0 (23) = happyShift action_10
action_0 (28) = happyShift action_11
action_0 (34) = happyShift action_12
action_0 (36) = happyShift action_13
action_0 (4) = happyGoto action_14
action_0 (5) = happyGoto action_15
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 (8) = happyGoto action_5
action_0 (9) = happyGoto action_6
action_0 (10) = happyGoto action_7
action_0 (11) = happyGoto action_8
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (22) = happyShift action_9
action_1 (23) = happyShift action_10
action_1 (28) = happyShift action_11
action_1 (34) = happyShift action_12
action_1 (36) = happyShift action_13
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 (8) = happyGoto action_5
action_1 (9) = happyGoto action_6
action_1 (10) = happyGoto action_7
action_1 (11) = happyGoto action_8
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_8

action_4 _ = happyReduce_3

action_5 _ = happyReduce_4

action_6 _ = happyReduce_5

action_7 _ = happyReduce_6

action_8 _ = happyReduce_7

action_9 (47) = happyShift action_26
action_9 (53) = happyShift action_27
action_9 (12) = happyGoto action_24
action_9 (13) = happyGoto action_25
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (54) = happyShift action_23
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (29) = happyShift action_22
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (43) = happyShift action_20
action_12 (14) = happyGoto action_21
action_12 (15) = happyGoto action_18
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (29) = happyShift action_19
action_13 (43) = happyShift action_20
action_13 (14) = happyGoto action_17
action_13 (15) = happyGoto action_18
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (56) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (22) = happyShift action_9
action_15 (23) = happyShift action_10
action_15 (28) = happyShift action_11
action_15 (34) = happyShift action_12
action_15 (36) = happyShift action_13
action_15 (4) = happyGoto action_16
action_15 (5) = happyGoto action_15
action_15 (6) = happyGoto action_3
action_15 (7) = happyGoto action_4
action_15 (8) = happyGoto action_5
action_15 (9) = happyGoto action_6
action_15 (10) = happyGoto action_7
action_15 (11) = happyGoto action_8
action_15 _ = happyReduce_1

action_16 _ = happyReduce_2

action_17 (24) = happyShift action_37
action_17 _ = happyReduce_17

action_18 (37) = happyShift action_36
action_18 _ = happyReduce_22

action_19 (31) = happyShift action_35
action_19 _ = happyFail (happyExpListPerState 19)

action_20 (53) = happyShift action_34
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (35) = happyShift action_33
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (23) = happyShift action_32
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_9

action_24 (23) = happyShift action_31
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (37) = happyShift action_30
action_25 _ = happyReduce_19

action_26 (23) = happyShift action_29
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (45) = happyShift action_28
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (53) = happyShift action_53
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (43) = happyShift action_20
action_29 (14) = happyGoto action_52
action_29 (15) = happyGoto action_18
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (53) = happyShift action_27
action_30 (12) = happyGoto action_51
action_30 (13) = happyGoto action_25
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (43) = happyShift action_20
action_31 (14) = happyGoto action_50
action_31 (15) = happyGoto action_18
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (43) = happyShift action_20
action_32 (15) = happyGoto action_49
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (53) = happyShift action_27
action_33 (13) = happyGoto action_46
action_33 (19) = happyGoto action_47
action_33 (20) = happyGoto action_48
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (44) = happyShift action_45
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (46) = happyShift action_44
action_35 (16) = happyGoto action_43
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (43) = happyShift action_20
action_36 (14) = happyGoto action_42
action_36 (15) = happyGoto action_18
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (33) = happyShift action_40
action_37 (43) = happyShift action_41
action_37 (53) = happyShift action_27
action_37 (13) = happyGoto action_38
action_37 (18) = happyGoto action_39
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (32) = happyShift action_66
action_38 (38) = happyShift action_67
action_38 (39) = happyShift action_68
action_38 (40) = happyShift action_69
action_38 (41) = happyShift action_70
action_38 (42) = happyShift action_71
action_38 (52) = happyShift action_72
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (25) = happyShift action_64
action_39 (26) = happyShift action_65
action_39 _ = happyReduce_16

action_40 (33) = happyShift action_40
action_40 (43) = happyShift action_41
action_40 (53) = happyShift action_27
action_40 (13) = happyGoto action_38
action_40 (18) = happyGoto action_63
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (33) = happyShift action_40
action_41 (43) = happyShift action_62
action_41 (53) = happyShift action_27
action_41 (13) = happyGoto action_38
action_41 (15) = happyGoto action_60
action_41 (18) = happyGoto action_61
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_23

action_43 _ = happyReduce_18

action_44 (53) = happyShift action_59
action_44 _ = happyFail (happyExpListPerState 44)

action_45 _ = happyReduce_24

action_46 (38) = happyShift action_58
action_46 _ = happyFail (happyExpListPerState 46)

action_47 _ = happyReduce_15

action_48 (37) = happyShift action_57
action_48 _ = happyReduce_39

action_49 (30) = happyShift action_56
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (24) = happyShift action_55
action_50 _ = happyReduce_11

action_51 _ = happyReduce_20

action_52 (24) = happyShift action_54
action_52 _ = happyReduce_13

action_53 _ = happyReduce_21

action_54 (33) = happyShift action_40
action_54 (43) = happyShift action_41
action_54 (53) = happyShift action_27
action_54 (13) = happyGoto action_38
action_54 (18) = happyGoto action_92
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (33) = happyShift action_40
action_55 (43) = happyShift action_41
action_55 (53) = happyShift action_27
action_55 (13) = happyGoto action_38
action_55 (18) = happyGoto action_91
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (43) = happyShift action_20
action_56 (15) = happyGoto action_90
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (53) = happyShift action_27
action_57 (13) = happyGoto action_46
action_57 (19) = happyGoto action_89
action_57 (20) = happyGoto action_48
action_57 _ = happyFail (happyExpListPerState 57)

action_58 (53) = happyShift action_27
action_58 (54) = happyShift action_76
action_58 (55) = happyShift action_77
action_58 (13) = happyGoto action_74
action_58 (21) = happyGoto action_88
action_58 _ = happyFail (happyExpListPerState 58)

action_59 _ = happyReduce_25

action_60 (44) = happyShift action_87
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (25) = happyShift action_64
action_61 (26) = happyShift action_65
action_61 (44) = happyShift action_86
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (33) = happyShift action_40
action_62 (43) = happyShift action_62
action_62 (53) = happyShift action_85
action_62 (13) = happyGoto action_38
action_62 (15) = happyGoto action_60
action_62 (18) = happyGoto action_61
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (25) = happyShift action_64
action_63 (26) = happyShift action_65
action_63 _ = happyReduce_29

action_64 (33) = happyShift action_40
action_64 (43) = happyShift action_41
action_64 (53) = happyShift action_27
action_64 (13) = happyGoto action_38
action_64 (18) = happyGoto action_84
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (33) = happyShift action_40
action_65 (43) = happyShift action_41
action_65 (53) = happyShift action_27
action_65 (13) = happyGoto action_38
action_65 (18) = happyGoto action_83
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (53) = happyShift action_27
action_66 (54) = happyShift action_76
action_66 (55) = happyShift action_77
action_66 (13) = happyGoto action_74
action_66 (21) = happyGoto action_82
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (53) = happyShift action_27
action_67 (54) = happyShift action_76
action_67 (55) = happyShift action_77
action_67 (13) = happyGoto action_74
action_67 (21) = happyGoto action_81
action_67 _ = happyFail (happyExpListPerState 67)

action_68 (53) = happyShift action_27
action_68 (54) = happyShift action_76
action_68 (55) = happyShift action_77
action_68 (13) = happyGoto action_74
action_68 (21) = happyGoto action_80
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (53) = happyShift action_27
action_69 (54) = happyShift action_76
action_69 (55) = happyShift action_77
action_69 (13) = happyGoto action_74
action_69 (21) = happyGoto action_79
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (53) = happyShift action_27
action_70 (54) = happyShift action_76
action_70 (55) = happyShift action_77
action_70 (13) = happyGoto action_74
action_70 (21) = happyGoto action_78
action_70 _ = happyFail (happyExpListPerState 70)

action_71 (53) = happyShift action_27
action_71 (54) = happyShift action_76
action_71 (55) = happyShift action_77
action_71 (13) = happyGoto action_74
action_71 (21) = happyGoto action_75
action_71 _ = happyFail (happyExpListPerState 71)

action_72 (54) = happyShift action_73
action_72 _ = happyFail (happyExpListPerState 72)

action_73 _ = happyReduce_36

action_74 _ = happyReduce_44

action_75 _ = happyReduce_34

action_76 _ = happyReduce_43

action_77 _ = happyReduce_42

action_78 _ = happyReduce_33

action_79 _ = happyReduce_32

action_80 _ = happyReduce_31

action_81 _ = happyReduce_30

action_82 _ = happyReduce_35

action_83 (25) = happyShift action_64
action_83 (26) = happyShift action_65
action_83 _ = happyReduce_28

action_84 (25) = happyShift action_64
action_84 (26) = happyShift action_65
action_84 _ = happyReduce_27

action_85 (44) = happyShift action_45
action_85 (45) = happyShift action_28
action_85 _ = happyFail (happyExpListPerState 85)

action_86 _ = happyReduce_38

action_87 (51) = happyShift action_94
action_87 _ = happyFail (happyExpListPerState 87)

action_88 _ = happyReduce_41

action_89 _ = happyReduce_40

action_90 (31) = happyShift action_93
action_90 _ = happyFail (happyExpListPerState 90)

action_91 (25) = happyShift action_64
action_91 (26) = happyShift action_65
action_91 _ = happyReduce_10

action_92 (25) = happyShift action_64
action_92 (26) = happyShift action_65
action_92 _ = happyReduce_12

action_93 (46) = happyShift action_44
action_93 (16) = happyGoto action_97
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (49) = happyShift action_96
action_94 (17) = happyGoto action_95
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (48) = happyShift action_99
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (43) = happyShift action_20
action_96 (15) = happyGoto action_98
action_96 _ = happyFail (happyExpListPerState 96)

action_97 _ = happyReduce_14

action_98 (51) = happyShift action_101
action_98 _ = happyFail (happyExpListPerState 98)

action_99 (43) = happyShift action_100
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (43) = happyShift action_20
action_100 (15) = happyGoto action_103
action_100 _ = happyFail (happyExpListPerState 100)

action_101 (46) = happyShift action_44
action_101 (16) = happyGoto action_102
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (48) = happyShift action_105
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (44) = happyShift action_104
action_103 _ = happyFail (happyExpListPerState 103)

action_104 _ = happyReduce_37

action_105 (43) = happyShift action_20
action_105 (15) = happyGoto action_106
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (50) = happyShift action_107
action_106 _ = happyFail (happyExpListPerState 106)

action_107 _ = happyReduce_26

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  5 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  5 happyReduction_6
happyReduction_6 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  6 happyReduction_9
happyReduction_9 (HappyTerminal (Tok _ (TokString happy_var_2)))
	_
	 =  HappyAbsSyn6
		 (SelectFile happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 6 7 happyReduction_10
happyReduction_10 ((HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (SelectQuery happy_var_2 happy_var_4 (Just happy_var_6)
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 7 happyReduction_11
happyReduction_11 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (SelectQuery happy_var_2 happy_var_4 Nothing
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 6 7 happyReduction_12
happyReduction_12 ((HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (SelectQuery [PropertyRef "*" "*"] happy_var_4 (Just happy_var_6)
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 4 7 happyReduction_13
happyReduction_13 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (SelectQuery [PropertyRef "*" "*"] happy_var_4 Nothing
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 8 8 happyReduction_14
happyReduction_14 ((HappyAbsSyn16  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (CreateEdgeQuery happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 4 9 happyReduction_15
happyReduction_15 ((HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (UpdateQuery happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 4 10 happyReduction_16
happyReduction_16 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (DeleteQuery happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_2  10 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (DeleteQuery happy_var_2 Nothing
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 11 happyReduction_18
happyReduction_18 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (DeleteEdgeQuery happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  12 happyReduction_20
happyReduction_20 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  13 happyReduction_21
happyReduction_21 (HappyTerminal (Tok _ (TokIdent happy_var_3)))
	_
	(HappyTerminal (Tok _ (TokIdent happy_var_1)))
	 =  HappyAbsSyn13
		 (PropertyRef happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  14 happyReduction_22
happyReduction_22 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  14 happyReduction_23
happyReduction_23 (HappyAbsSyn14  happy_var_3)
	_
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 : happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  15 happyReduction_24
happyReduction_24 _
	(HappyTerminal (Tok _ (TokIdent happy_var_2)))
	_
	 =  HappyAbsSyn15
		 (NodePattern happy_var_2
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  16 happyReduction_25
happyReduction_25 (HappyTerminal (Tok _ (TokIdent happy_var_2)))
	_
	 =  HappyAbsSyn16
		 (EdgeType happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happyReduce 7 17 happyReduction_26
happyReduction_26 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn17
		 (EdgePattern happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_3  18 happyReduction_27
happyReduction_27 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (ConditionAnd happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  18 happyReduction_28
happyReduction_28 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (ConditionOr happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_2  18 happyReduction_29
happyReduction_29 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (ConditionNot happy_var_2
	)
happyReduction_29 _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  18 happyReduction_30
happyReduction_30 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn18
		 (ConditionEqual happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  18 happyReduction_31
happyReduction_31 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn18
		 (ConditionLess happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  18 happyReduction_32
happyReduction_32 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn18
		 (ConditionGreater happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  18 happyReduction_33
happyReduction_33 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn18
		 (ConditionLessEqual happy_var_1 happy_var_3
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  18 happyReduction_34
happyReduction_34 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn18
		 (ConditionGreaterEqual happy_var_1 happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_3  18 happyReduction_35
happyReduction_35 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn18
		 (ConditionStartsWith happy_var_1 happy_var_3
	)
happyReduction_35 _ _ _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  18 happyReduction_36
happyReduction_36 (HappyTerminal (Tok _ (TokString happy_var_3)))
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn18
		 (ConditionHasLabel happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happyReduce 9 18 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn15  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn17  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (ConditionEdge happy_var_2 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_3  18 happyReduction_38
happyReduction_38 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  19 happyReduction_39
happyReduction_39 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  19 happyReduction_40
happyReduction_40 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 : happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  20 happyReduction_41
happyReduction_41 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn20
		 (PropertyUpdate happy_var_1 happy_var_3
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  21 happyReduction_42
happyReduction_42 (HappyTerminal (Tok _ (TokInt happy_var_1)))
	 =  HappyAbsSyn21
		 (ExpressionInteger happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  21 happyReduction_43
happyReduction_43 (HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn21
		 (ExpressionString happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  21 happyReduction_44
happyReduction_44 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn21
		 (ExpressionPropertyRef happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 56 56 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tok _ TokSelect -> cont 22;
	Tok _ TokFrom -> cont 23;
	Tok _ TokWhere -> cont 24;
	Tok _ TokAnd -> cont 25;
	Tok _ TokOr -> cont 26;
	Tok _ TokLimit -> cont 27;
	Tok _ TokCreate -> cont 28;
	Tok _ TokEdge -> cont 29;
	Tok _ TokTo -> cont 30;
	Tok _ TokType -> cont 31;
	Tok _ TokStartsWith -> cont 32;
	Tok _ TokNot -> cont 33;
	Tok _ TokUpdate -> cont 34;
	Tok _ TokSet -> cont 35;
	Tok _ TokDelete -> cont 36;
	Tok _ TokComma -> cont 37;
	Tok _ TokEqual -> cont 38;
	Tok _ TokLT -> cont 39;
	Tok _ TokGT -> cont 40;
	Tok _ TokLTE -> cont 41;
	Tok _ TokGTE -> cont 42;
	Tok _ TokLParen -> cont 43;
	Tok _ TokRParen -> cont 44;
	Tok _ TokPeriod -> cont 45;
	Tok _ TokColon -> cont 46;
	Tok _ TokAsterisk -> cont 47;
	Tok _ TokArrow -> cont 48;
	Tok _ TokLBracket -> cont 49;
	Tok _ TokRBracket -> cont 50;
	Tok _ TokMinus -> cont 51;
	Tok _ TokHas -> cont 52;
	Tok _ (TokIdent happy_dollar_dollar) -> cont 53;
	Tok _ (TokString happy_dollar_dollar) -> cont 54;
	Tok _ (TokInt happy_dollar_dollar) -> cont 55;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 56 tk tks = happyError' (tks, explist)
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
