{-# OPTIONS_GHC -w #-}
module InputParser where
import InputLexer (Token(..), TokenType(..), tokenPosn)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 t16 t17
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,79) ([0,0,0,0,0,0,16,0,0,0,0,0,24,0,0,0,0,0,0,6,0,0,0,0,0,0,0,0,0,0,0,0,1024,0,8192,0,17920,0,12288,2,0,64,0,512,0,9120,0,7936,1,0,0,0,8,0,64,0,2,0,32,0,0,0,0,16,1792,0,14336,0,0,8192,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2048,0,0,16,0,0,0,16,0,32768,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,0,2,0,0,0,0,0,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseInput","Tables","Table","Header","Types","Type","Rows","Row","ID","StartID","EndID","Labels","Relationship","Values","Value","stringType","intType","boolType","id","startId","endId","label","type","string","alphanum","int","bool","null","':'","';'","','","'\"'","%eof"]
        bit_start = st Prelude.* 35
        bit_end = (st Prelude.+ 1) Prelude.* 35
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..34]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (4) = happyGoto action_2
action_0 _ = happyReduce_1

action_1 _ = happyFail (happyExpListPerState 1)

action_2 (31) = happyShift action_5
action_2 (35) = happyAccept
action_2 (5) = happyGoto action_3
action_2 (6) = happyGoto action_4
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_2

action_4 (9) = happyGoto action_8
action_4 _ = happyReduce_15

action_5 (21) = happyShift action_6
action_5 (22) = happyShift action_7
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (7) = happyGoto action_15
action_6 _ = happyReduce_7

action_7 (7) = happyGoto action_14
action_7 _ = happyReduce_7

action_8 (26) = happyShift action_12
action_8 (27) = happyShift action_13
action_8 (10) = happyGoto action_9
action_8 (11) = happyGoto action_10
action_8 (12) = happyGoto action_11
action_8 _ = happyReduce_3

action_9 _ = happyReduce_16

action_10 (16) = happyGoto action_19
action_10 _ = happyReduce_29

action_11 (16) = happyGoto action_18
action_11 _ = happyReduce_29

action_12 (33) = happyReduce_22
action_12 _ = happyReduce_20

action_13 (33) = happyReduce_23
action_13 _ = happyReduce_21

action_14 (33) = happyShift action_17
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (33) = happyShift action_16
action_15 _ = happyReduce_4

action_16 (26) = happyShift action_23
action_16 (27) = happyShift action_24
action_16 (31) = happyShift action_26
action_16 (8) = happyGoto action_22
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (26) = happyShift action_23
action_17 (27) = happyShift action_24
action_17 (31) = happyShift action_25
action_17 (8) = happyGoto action_22
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (33) = happyShift action_21
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (33) = happyShift action_20
action_19 _ = happyReduce_17

action_20 (26) = happyShift action_40
action_20 (28) = happyShift action_35
action_20 (29) = happyShift action_36
action_20 (30) = happyShift action_37
action_20 (34) = happyShift action_38
action_20 (14) = happyGoto action_39
action_20 (17) = happyGoto action_32
action_20 _ = happyFail (happyExpListPerState 20)

action_21 (26) = happyShift action_33
action_21 (27) = happyShift action_34
action_21 (28) = happyShift action_35
action_21 (29) = happyShift action_36
action_21 (30) = happyShift action_37
action_21 (34) = happyShift action_38
action_21 (13) = happyGoto action_31
action_21 (17) = happyGoto action_32
action_21 _ = happyFail (happyExpListPerState 21)

action_22 _ = happyReduce_8

action_23 (31) = happyShift action_30
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (31) = happyShift action_29
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (23) = happyShift action_28
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (24) = happyShift action_27
action_26 _ = happyFail (happyExpListPerState 26)

action_27 _ = happyReduce_5

action_28 (33) = happyShift action_50
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (18) = happyShift action_47
action_29 (19) = happyShift action_48
action_29 (20) = happyShift action_49
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (18) = happyShift action_44
action_30 (19) = happyShift action_45
action_30 (20) = happyShift action_46
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (33) = happyShift action_43
action_31 _ = happyFail (happyExpListPerState 31)

action_32 _ = happyReduce_30

action_33 _ = happyReduce_24

action_34 _ = happyReduce_25

action_35 _ = happyReduce_32

action_36 _ = happyReduce_33

action_37 _ = happyReduce_34

action_38 (26) = happyShift action_42
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (32) = happyShift action_41
action_39 _ = happyReduce_18

action_40 _ = happyReduce_26

action_41 (26) = happyShift action_55
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (34) = happyShift action_54
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (26) = happyShift action_53
action_43 (15) = happyGoto action_52
action_43 _ = happyFail (happyExpListPerState 43)

action_44 _ = happyReduce_9

action_45 _ = happyReduce_11

action_46 _ = happyReduce_13

action_47 _ = happyReduce_10

action_48 _ = happyReduce_12

action_49 _ = happyReduce_14

action_50 (31) = happyShift action_51
action_50 _ = happyFail (happyExpListPerState 50)

action_51 (25) = happyShift action_56
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_19

action_53 _ = happyReduce_28

action_54 _ = happyReduce_31

action_55 _ = happyReduce_27

action_56 _ = happyReduce_6

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 ([]
	)

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_2 : happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 (HappyAbsSyn7  happy_var_3)
	_
	_
	 =  HappyAbsSyn6
		 (Header happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 6 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (LabeledHeader happy_var_3
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 9 6 happyReduction_6
happyReduction_6 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (RelationshipHeader happy_var_3
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_0  7 happyReduction_7
happyReduction_7  =  HappyAbsSyn7
		 ([]
	)

happyReduce_8 = happySpecReduce_3  7 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_3 : happy_var_1
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 _
	_
	(HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn8
		 (StringType happy_var_1
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 _
	_
	(HappyTerminal (Tok _ (TokAlphaNum happy_var_1)))
	 =  HappyAbsSyn8
		 (StringType happy_var_1
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  8 happyReduction_11
happyReduction_11 _
	_
	(HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn8
		 (IntType happy_var_1
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 _
	_
	(HappyTerminal (Tok _ (TokAlphaNum happy_var_1)))
	 =  HappyAbsSyn8
		 (IntType happy_var_1
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 _
	_
	(HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn8
		 (BoolType happy_var_1
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  8 happyReduction_14
happyReduction_14 _
	_
	(HappyTerminal (Tok _ (TokAlphaNum happy_var_1)))
	 =  HappyAbsSyn8
		 (BoolType happy_var_1
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_0  9 happyReduction_15
happyReduction_15  =  HappyAbsSyn9
		 ([]
	)

happyReduce_16 = happySpecReduce_2  9 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_2 : happy_var_1
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_2  10 happyReduction_17
happyReduction_17 (HappyAbsSyn16  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (Data happy_var_1 happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 4 10 happyReduction_18
happyReduction_18 ((HappyAbsSyn14  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (LabeledData happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 6 10 happyReduction_19
happyReduction_19 ((HappyAbsSyn15  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_2) `HappyStk`
	(HappyAbsSyn12  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (RelationshipData happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1  11 happyReduction_20
happyReduction_20 (HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn11
		 (Id happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  11 happyReduction_21
happyReduction_21 (HappyTerminal (Tok _ (TokAlphaNum happy_var_1)))
	 =  HappyAbsSyn11
		 (Id happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  12 happyReduction_22
happyReduction_22 (HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn12
		 (StartID happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  12 happyReduction_23
happyReduction_23 (HappyTerminal (Tok _ (TokAlphaNum happy_var_1)))
	 =  HappyAbsSyn12
		 (StartID happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  13 happyReduction_24
happyReduction_24 (HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn13
		 (EndID happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  13 happyReduction_25
happyReduction_25 (HappyTerminal (Tok _ (TokAlphaNum happy_var_1)))
	 =  HappyAbsSyn13
		 (EndID happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  14 happyReduction_26
happyReduction_26 (HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn14
		 ([happy_var_1]
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  14 happyReduction_27
happyReduction_27 (HappyTerminal (Tok _ (TokString happy_var_3)))
	_
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_3 : happy_var_1
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  15 happyReduction_28
happyReduction_28 (HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn15
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_0  16 happyReduction_29
happyReduction_29  =  HappyAbsSyn16
		 ([]
	)

happyReduce_30 = happySpecReduce_3  16 happyReduction_30
happyReduction_30 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_3 : happy_var_1
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  17 happyReduction_31
happyReduction_31 _
	(HappyTerminal (Tok _ (TokString happy_var_2)))
	_
	 =  HappyAbsSyn17
		 (StringValue happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  17 happyReduction_32
happyReduction_32 (HappyTerminal (Tok _ (TokInt happy_var_1)))
	 =  HappyAbsSyn17
		 (IntValue happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  17 happyReduction_33
happyReduction_33 (HappyTerminal (Tok _ (TokBool happy_var_1)))
	 =  HappyAbsSyn17
		 (BoolValue happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  17 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn17
		 (NullValue
	)

happyNewToken action sts stk [] =
	action 35 35 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tok _ TokStringType -> cont 18;
	Tok _ TokIntType -> cont 19;
	Tok _ TokBoolType -> cont 20;
	Tok _ TokId -> cont 21;
	Tok _ TokStartId -> cont 22;
	Tok _ TokEndId -> cont 23;
	Tok _ TokLabel -> cont 24;
	Tok _ TokType -> cont 25;
	Tok _ (TokString happy_dollar_dollar) -> cont 26;
	Tok _ (TokAlphaNum happy_dollar_dollar) -> cont 27;
	Tok _ (TokInt happy_dollar_dollar) -> cont 28;
	Tok _ (TokBool happy_dollar_dollar) -> cont 29;
	Tok _ TokNull -> cont 30;
	Tok _ TokColon -> cont 31;
	Tok _ TokSemiColon -> cont 32;
	Tok _ TokComma -> cont 33;
	Tok _ TokQuoteMark -> cont 34;
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
parseInput tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError tokens@(x:xs) = error $ "Parse error: " ++ tokenPosn (head tokens) ++ "\n" ++ show tokens
parseError [] = error "Parse error: end of input"

type Tables = [Table]

type Table = [Row]

type Types = [Type]

type Values = [Value]

data ID = Id String 
        | StartID String 
        | EndID String
        deriving (Eq,Show)

type Relationship = String

type Labels = [String]

data Row = 
    Header Types |
    LabeledHeader Types |
    RelationshipHeader Types |
    Data ID Values |
    LabeledData ID Values Labels |
    RelationshipData ID Values ID Relationship
    deriving (Eq,Show)

data Type = 
    StringType String |
    IntType String |
    BoolType String
    deriving (Eq,Show)

data Value =
    NullValue |
    StringValue String |
    IntValue Int |
    BoolValue Bool
    deriving (Eq,Show)
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
