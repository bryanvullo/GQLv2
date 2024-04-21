{-# OPTIONS_GHC -w #-}
module Parser where

import Lexer
import Syntax
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17 t18 t19
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,188) ([0,33288,2,0,512,0,0,0,0,0,0,0,16384,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,16,0,4096,1024,0,8192,0,0,0,0,1,0,0,1,0,0,0,4096,0,0,1,0,1024,0,0,0,1,0,0,0,16,0,4096,0,0,0,0,256,0,0,0,64,0,0,4,0,0,0,4,0,16384,0,0,0,16,0,0,0,16,0,0,2,0,0,512,0,0,4096,0,0,256,4100,0,8192,248,2,12288,0,0,0,1024,16400,0,0,1025,16,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,8,0,0,0,0,0,16384,0,0,8192,0,0,8192,0,0,0,0,0,0,512,0,0,0,0,0,0,16384,256,4,0,16400,256,0,0,16,0,0,0,16,0,0,7168,0,0,0,0,0,8192,0,0,3,8,0,16384,256,4,0,0,0,0,1024,16400,0,0,1025,16,0,0,7168,0,0,0,7,0,0,448,0,0,28672,0,0,0,28,0,0,1792,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,16,0,0,48,0,0,3072,0,0,0,0,32,0,0,16384,0,0,0,8,0,0,16,0,0,0,0,0,0,256,0,0,64,0,0,4096,0,0,0,32,0,0,8192,0,0,32768,0,0,0,0,0,0,1024,0,0,0,128,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parse","Query","SelectQuery","CreateEdgeQuery","UpdateQuery","DeleteQuery","DeleteEdgeQuery","PropertyRefs","PropertyRef","NodePatterns","NodePattern","EdgeType","EdgePattern","Condition","PropertyUpdates","PropertyUpdate","Expression","SELECT","FROM","WHERE","AND","OR","LIMIT","CREATE","EDGE","TO","TYPE","STARTS_WITH","NOT","UPDATE","SET","DELETE","','","'='","'<'","'>'","'<='","'>='","'('","')'","'.'","':'","'*'","'->'","'['","']'","'-'","HAS","IDENT","STRING","INT","%eof"]
        bit_start = st Prelude.* 54
        bit_end = (st Prelude.+ 1) Prelude.* 54
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..53]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (20) = happyShift action_3
action_0 (26) = happyShift action_9
action_0 (32) = happyShift action_10
action_0 (34) = happyShift action_11
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_7
action_0 (9) = happyGoto action_8
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (20) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail (happyExpListPerState 1)

action_2 _ = happyReduce_1

action_3 (45) = happyShift action_20
action_3 (51) = happyShift action_21
action_3 (10) = happyGoto action_18
action_3 (11) = happyGoto action_19
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (54) = happyAccept
action_4 _ = happyFail (happyExpListPerState 4)

action_5 _ = happyReduce_2

action_6 _ = happyReduce_3

action_7 _ = happyReduce_4

action_8 _ = happyReduce_5

action_9 (27) = happyShift action_17
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (41) = happyShift action_15
action_10 (12) = happyGoto action_16
action_10 (13) = happyGoto action_13
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (27) = happyShift action_14
action_11 (41) = happyShift action_15
action_11 (12) = happyGoto action_12
action_11 (13) = happyGoto action_13
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (22) = happyShift action_31
action_12 _ = happyReduce_13

action_13 (35) = happyShift action_30
action_13 _ = happyReduce_18

action_14 (29) = happyShift action_29
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (51) = happyShift action_28
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (33) = happyShift action_27
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (21) = happyShift action_26
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (21) = happyShift action_25
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (35) = happyShift action_24
action_19 _ = happyReduce_15

action_20 (21) = happyShift action_23
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (43) = happyShift action_22
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (51) = happyShift action_47
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (41) = happyShift action_15
action_23 (12) = happyGoto action_46
action_23 (13) = happyGoto action_13
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (51) = happyShift action_21
action_24 (10) = happyGoto action_45
action_24 (11) = happyGoto action_19
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (41) = happyShift action_15
action_25 (12) = happyGoto action_44
action_25 (13) = happyGoto action_13
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (41) = happyShift action_15
action_26 (13) = happyGoto action_43
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (51) = happyShift action_21
action_27 (11) = happyGoto action_40
action_27 (17) = happyGoto action_41
action_27 (18) = happyGoto action_42
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (42) = happyShift action_39
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (44) = happyShift action_38
action_29 (14) = happyGoto action_37
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (41) = happyShift action_15
action_30 (12) = happyGoto action_36
action_30 (13) = happyGoto action_13
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (31) = happyShift action_34
action_31 (41) = happyShift action_35
action_31 (51) = happyShift action_21
action_31 (11) = happyGoto action_32
action_31 (16) = happyGoto action_33
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (30) = happyShift action_60
action_32 (36) = happyShift action_61
action_32 (37) = happyShift action_62
action_32 (38) = happyShift action_63
action_32 (39) = happyShift action_64
action_32 (40) = happyShift action_65
action_32 (50) = happyShift action_66
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (23) = happyShift action_58
action_33 (24) = happyShift action_59
action_33 _ = happyReduce_12

action_34 (31) = happyShift action_34
action_34 (41) = happyShift action_35
action_34 (51) = happyShift action_21
action_34 (11) = happyGoto action_32
action_34 (16) = happyGoto action_57
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (31) = happyShift action_34
action_35 (41) = happyShift action_56
action_35 (51) = happyShift action_21
action_35 (11) = happyGoto action_32
action_35 (13) = happyGoto action_54
action_35 (16) = happyGoto action_55
action_35 _ = happyFail (happyExpListPerState 35)

action_36 _ = happyReduce_19

action_37 _ = happyReduce_14

action_38 (51) = happyShift action_53
action_38 _ = happyFail (happyExpListPerState 38)

action_39 _ = happyReduce_20

action_40 (36) = happyShift action_52
action_40 _ = happyFail (happyExpListPerState 40)

action_41 _ = happyReduce_11

action_42 (35) = happyShift action_51
action_42 _ = happyReduce_35

action_43 (28) = happyShift action_50
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (22) = happyShift action_49
action_44 _ = happyReduce_7

action_45 _ = happyReduce_16

action_46 (22) = happyShift action_48
action_46 _ = happyReduce_9

action_47 _ = happyReduce_17

action_48 (31) = happyShift action_34
action_48 (41) = happyShift action_35
action_48 (51) = happyShift action_21
action_48 (11) = happyGoto action_32
action_48 (16) = happyGoto action_86
action_48 _ = happyFail (happyExpListPerState 48)

action_49 (31) = happyShift action_34
action_49 (41) = happyShift action_35
action_49 (51) = happyShift action_21
action_49 (11) = happyGoto action_32
action_49 (16) = happyGoto action_85
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (41) = happyShift action_15
action_50 (13) = happyGoto action_84
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (51) = happyShift action_21
action_51 (11) = happyGoto action_40
action_51 (17) = happyGoto action_83
action_51 (18) = happyGoto action_42
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (51) = happyShift action_21
action_52 (52) = happyShift action_70
action_52 (53) = happyShift action_71
action_52 (11) = happyGoto action_68
action_52 (19) = happyGoto action_82
action_52 _ = happyFail (happyExpListPerState 52)

action_53 _ = happyReduce_21

action_54 (42) = happyShift action_81
action_54 _ = happyFail (happyExpListPerState 54)

action_55 (23) = happyShift action_58
action_55 (24) = happyShift action_59
action_55 (42) = happyShift action_80
action_55 _ = happyFail (happyExpListPerState 55)

action_56 (31) = happyShift action_34
action_56 (41) = happyShift action_56
action_56 (51) = happyShift action_79
action_56 (11) = happyGoto action_32
action_56 (13) = happyGoto action_54
action_56 (16) = happyGoto action_55
action_56 _ = happyFail (happyExpListPerState 56)

action_57 (23) = happyShift action_58
action_57 (24) = happyShift action_59
action_57 _ = happyReduce_25

action_58 (31) = happyShift action_34
action_58 (41) = happyShift action_35
action_58 (51) = happyShift action_21
action_58 (11) = happyGoto action_32
action_58 (16) = happyGoto action_78
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (31) = happyShift action_34
action_59 (41) = happyShift action_35
action_59 (51) = happyShift action_21
action_59 (11) = happyGoto action_32
action_59 (16) = happyGoto action_77
action_59 _ = happyFail (happyExpListPerState 59)

action_60 (51) = happyShift action_21
action_60 (52) = happyShift action_70
action_60 (53) = happyShift action_71
action_60 (11) = happyGoto action_68
action_60 (19) = happyGoto action_76
action_60 _ = happyFail (happyExpListPerState 60)

action_61 (51) = happyShift action_21
action_61 (52) = happyShift action_70
action_61 (53) = happyShift action_71
action_61 (11) = happyGoto action_68
action_61 (19) = happyGoto action_75
action_61 _ = happyFail (happyExpListPerState 61)

action_62 (51) = happyShift action_21
action_62 (52) = happyShift action_70
action_62 (53) = happyShift action_71
action_62 (11) = happyGoto action_68
action_62 (19) = happyGoto action_74
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (51) = happyShift action_21
action_63 (52) = happyShift action_70
action_63 (53) = happyShift action_71
action_63 (11) = happyGoto action_68
action_63 (19) = happyGoto action_73
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (51) = happyShift action_21
action_64 (52) = happyShift action_70
action_64 (53) = happyShift action_71
action_64 (11) = happyGoto action_68
action_64 (19) = happyGoto action_72
action_64 _ = happyFail (happyExpListPerState 64)

action_65 (51) = happyShift action_21
action_65 (52) = happyShift action_70
action_65 (53) = happyShift action_71
action_65 (11) = happyGoto action_68
action_65 (19) = happyGoto action_69
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (52) = happyShift action_67
action_66 _ = happyFail (happyExpListPerState 66)

action_67 _ = happyReduce_32

action_68 _ = happyReduce_40

action_69 _ = happyReduce_30

action_70 _ = happyReduce_39

action_71 _ = happyReduce_38

action_72 _ = happyReduce_29

action_73 _ = happyReduce_28

action_74 _ = happyReduce_27

action_75 _ = happyReduce_26

action_76 _ = happyReduce_31

action_77 (23) = happyShift action_58
action_77 (24) = happyShift action_59
action_77 _ = happyReduce_24

action_78 (23) = happyShift action_58
action_78 (24) = happyShift action_59
action_78 _ = happyReduce_23

action_79 (42) = happyShift action_39
action_79 (43) = happyShift action_22
action_79 _ = happyFail (happyExpListPerState 79)

action_80 _ = happyReduce_34

action_81 (49) = happyShift action_88
action_81 _ = happyFail (happyExpListPerState 81)

action_82 _ = happyReduce_37

action_83 _ = happyReduce_36

action_84 (29) = happyShift action_87
action_84 _ = happyFail (happyExpListPerState 84)

action_85 (23) = happyShift action_58
action_85 (24) = happyShift action_59
action_85 _ = happyReduce_6

action_86 (23) = happyShift action_58
action_86 (24) = happyShift action_59
action_86 _ = happyReduce_8

action_87 (44) = happyShift action_38
action_87 (14) = happyGoto action_91
action_87 _ = happyFail (happyExpListPerState 87)

action_88 (47) = happyShift action_90
action_88 (15) = happyGoto action_89
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (46) = happyShift action_93
action_89 _ = happyFail (happyExpListPerState 89)

action_90 (41) = happyShift action_15
action_90 (13) = happyGoto action_92
action_90 _ = happyFail (happyExpListPerState 90)

action_91 _ = happyReduce_10

action_92 (49) = happyShift action_95
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (41) = happyShift action_94
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (41) = happyShift action_15
action_94 (13) = happyGoto action_97
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (44) = happyShift action_38
action_95 (14) = happyGoto action_96
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (46) = happyShift action_99
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (42) = happyShift action_98
action_97 _ = happyFail (happyExpListPerState 97)

action_98 _ = happyReduce_33

action_99 (41) = happyShift action_15
action_99 (13) = happyGoto action_100
action_99 _ = happyFail (happyExpListPerState 99)

action_100 (48) = happyShift action_101
action_100 _ = happyFail (happyExpListPerState 100)

action_101 _ = happyReduce_22

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  4 happyReduction_3
happyReduction_3 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  4 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 6 5 happyReduction_6
happyReduction_6 ((HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (SelectQuery happy_var_2 happy_var_4 (Just happy_var_6)
	) `HappyStk` happyRest

happyReduce_7 = happyReduce 4 5 happyReduction_7
happyReduction_7 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (SelectQuery happy_var_2 happy_var_4 Nothing
	) `HappyStk` happyRest

happyReduce_8 = happyReduce 6 5 happyReduction_8
happyReduction_8 ((HappyAbsSyn16  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (SelectQuery [PropertyRef "*" "*"] happy_var_4 (Just happy_var_6)
	) `HappyStk` happyRest

happyReduce_9 = happyReduce 4 5 happyReduction_9
happyReduction_9 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (SelectQuery [PropertyRef "*" "*"] happy_var_4 Nothing
	) `HappyStk` happyRest

happyReduce_10 = happyReduce 8 6 happyReduction_10
happyReduction_10 ((HappyAbsSyn14  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (CreateEdgeQuery happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_11 = happyReduce 4 7 happyReduction_11
happyReduction_11 ((HappyAbsSyn17  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (UpdateQuery happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 4 8 happyReduction_12
happyReduction_12 ((HappyAbsSyn16  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (DeleteQuery happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_13 = happySpecReduce_2  8 happyReduction_13
happyReduction_13 (HappyAbsSyn12  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (DeleteQuery happy_var_2 Nothing
	)
happyReduction_13 _ _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 9 happyReduction_14
happyReduction_14 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (DeleteEdgeQuery happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  10 happyReduction_15
happyReduction_15 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  11 happyReduction_17
happyReduction_17 (HappyTerminal (Tok _ (TokIdent happy_var_3)))
	_
	(HappyTerminal (Tok _ (TokIdent happy_var_1)))
	 =  HappyAbsSyn11
		 (PropertyRef happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_1  12 happyReduction_18
happyReduction_18 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_18 _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  12 happyReduction_19
happyReduction_19 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  13 happyReduction_20
happyReduction_20 _
	(HappyTerminal (Tok _ (TokIdent happy_var_2)))
	_
	 =  HappyAbsSyn13
		 (NodePattern happy_var_2
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  14 happyReduction_21
happyReduction_21 (HappyTerminal (Tok _ (TokIdent happy_var_2)))
	_
	 =  HappyAbsSyn14
		 (EdgeType happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 7 15 happyReduction_22
happyReduction_22 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (EdgePattern happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_3  16 happyReduction_23
happyReduction_23 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (ConditionAnd happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  16 happyReduction_24
happyReduction_24 (HappyAbsSyn16  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (ConditionOr happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_2  16 happyReduction_25
happyReduction_25 (HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (ConditionNot happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  16 happyReduction_26
happyReduction_26 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn16
		 (ConditionEqual happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  16 happyReduction_27
happyReduction_27 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn16
		 (ConditionLess happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  16 happyReduction_28
happyReduction_28 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn16
		 (ConditionGreater happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  16 happyReduction_29
happyReduction_29 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn16
		 (ConditionLessEqual happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  16 happyReduction_30
happyReduction_30 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn16
		 (ConditionGreaterEqual happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  16 happyReduction_31
happyReduction_31 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn16
		 (ConditionStartsWith happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  16 happyReduction_32
happyReduction_32 (HappyTerminal (Tok _ (TokString happy_var_3)))
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn16
		 (ConditionHasLabel happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happyReduce 9 16 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (ConditionEdge happy_var_2 happy_var_5 happy_var_8
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_3  16 happyReduction_34
happyReduction_34 _
	(HappyAbsSyn16  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (happy_var_2
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  17 happyReduction_35
happyReduction_35 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  17 happyReduction_36
happyReduction_36 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 : happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  18 happyReduction_37
happyReduction_37 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn18
		 (PropertyUpdate happy_var_1 happy_var_3
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  19 happyReduction_38
happyReduction_38 (HappyTerminal (Tok _ (TokInt happy_var_1)))
	 =  HappyAbsSyn19
		 (ExpressionInteger happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  19 happyReduction_39
happyReduction_39 (HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn19
		 (ExpressionString happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  19 happyReduction_40
happyReduction_40 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn19
		 (ExpressionPropertyRef happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 54 54 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tok _ TokSelect -> cont 20;
	Tok _ TokFrom -> cont 21;
	Tok _ TokWhere -> cont 22;
	Tok _ TokAnd -> cont 23;
	Tok _ TokOr -> cont 24;
	Tok _ TokLimit -> cont 25;
	Tok _ TokCreate -> cont 26;
	Tok _ TokEdge -> cont 27;
	Tok _ TokTo -> cont 28;
	Tok _ TokType -> cont 29;
	Tok _ TokStartsWith -> cont 30;
	Tok _ TokNot -> cont 31;
	Tok _ TokUpdate -> cont 32;
	Tok _ TokSet -> cont 33;
	Tok _ TokDelete -> cont 34;
	Tok _ TokComma -> cont 35;
	Tok _ TokEqual -> cont 36;
	Tok _ TokLT -> cont 37;
	Tok _ TokGT -> cont 38;
	Tok _ TokLTE -> cont 39;
	Tok _ TokGTE -> cont 40;
	Tok _ TokLParen -> cont 41;
	Tok _ TokRParen -> cont 42;
	Tok _ TokPeriod -> cont 43;
	Tok _ TokColon -> cont 44;
	Tok _ TokAsterisk -> cont 45;
	Tok _ TokArrow -> cont 46;
	Tok _ TokLBracket -> cont 47;
	Tok _ TokRBracket -> cont 48;
	Tok _ TokMinus -> cont 49;
	Tok _ TokHas -> cont 50;
	Tok _ (TokIdent happy_dollar_dollar) -> cont 51;
	Tok _ (TokString happy_dollar_dollar) -> cont 52;
	Tok _ (TokInt happy_dollar_dollar) -> cont 53;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 54 tk tks = happyError' (tks, explist)
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
