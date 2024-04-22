{-# OPTIONS_GHC -w #-}
module InputParser where
import InputLexer (Token(..), TokenType(..), tokenPosn)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.1.1

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,71) ([0,6,0,0,0,0,0,6,0,128,0,8192,0,8192,0,192,0,64,0,0,0,128,0,0,0,0,0,8192,0,0,0,18176,0,0,0,16,0,8192,0,2048,0,0,0,0,0,8,0,8192,49152,1,0,64,0,0,0,0,0,0,0,0,0,64,0,0,0,64,0,0,0,4096,0,8192,0,16384,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,64,0,64,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_parseInput","Tables","Table","Header","Types","Type","Name","Rows","Row","Labels","Values","Value","stringType","intType","boolType","id","startId","endId","label","type","string","alphanum","int","bool","null","':'","';'","','","'\"'","%eof"]
        bit_start = st Prelude.* 32
        bit_end = (st Prelude.+ 1) Prelude.* 32
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..31]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (18) = happyShift action_5
action_0 (19) = happyShift action_6
action_0 (4) = happyGoto action_2
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 _ = happyReduce_1

action_1 _ = happyFail (happyExpListPerState 1)

action_2 (32) = happyAccept
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (18) = happyShift action_5
action_3 (19) = happyShift action_6
action_3 (4) = happyGoto action_12
action_3 (5) = happyGoto action_3
action_3 (6) = happyGoto action_4
action_3 _ = happyReduce_1

action_4 (24) = happyShift action_11
action_4 (10) = happyGoto action_9
action_4 (11) = happyGoto action_10
action_4 _ = happyReduce_14

action_5 (30) = happyShift action_8
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (30) = happyShift action_7
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (23) = happyShift action_20
action_7 (24) = happyShift action_21
action_7 (7) = happyGoto action_22
action_7 (8) = happyGoto action_18
action_7 (9) = happyGoto action_19
action_7 _ = happyReduce_7

action_8 (23) = happyShift action_20
action_8 (24) = happyShift action_21
action_8 (7) = happyGoto action_17
action_8 (8) = happyGoto action_18
action_8 (9) = happyGoto action_19
action_8 _ = happyReduce_7

action_9 _ = happyReduce_3

action_10 (24) = happyShift action_11
action_10 (10) = happyGoto action_16
action_10 (11) = happyGoto action_10
action_10 _ = happyReduce_14

action_11 (30) = happyShift action_15
action_11 (13) = happyGoto action_13
action_11 (14) = happyGoto action_14
action_11 _ = happyReduce_22

action_12 _ = happyReduce_2

action_13 (30) = happyShift action_32
action_13 _ = happyReduce_16

action_14 (30) = happyShift action_15
action_14 (13) = happyGoto action_31
action_14 (14) = happyGoto action_14
action_14 _ = happyReduce_22

action_15 (25) = happyShift action_27
action_15 (26) = happyShift action_28
action_15 (27) = happyShift action_29
action_15 (31) = happyShift action_30
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_15

action_17 (21) = happyShift action_26
action_17 _ = happyReduce_4

action_18 (30) = happyShift action_25
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (28) = happyShift action_24
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_12

action_21 _ = happyReduce_13

action_22 (20) = happyShift action_23
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (30) = happyShift action_41
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (15) = happyShift action_38
action_24 (16) = happyShift action_39
action_24 (17) = happyShift action_40
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (23) = happyShift action_20
action_25 (24) = happyShift action_21
action_25 (7) = happyGoto action_37
action_25 (8) = happyGoto action_18
action_25 (9) = happyGoto action_19
action_25 _ = happyReduce_7

action_26 _ = happyReduce_5

action_27 _ = happyReduce_25

action_28 _ = happyReduce_26

action_29 _ = happyReduce_27

action_30 (23) = happyShift action_36
action_30 _ = happyFail (happyExpListPerState 30)

action_31 _ = happyReduce_23

action_32 (23) = happyShift action_34
action_32 (24) = happyShift action_35
action_32 (12) = happyGoto action_33
action_32 _ = happyReduce_19

action_33 _ = happyReduce_17

action_34 (29) = happyShift action_45
action_34 _ = happyReduce_20

action_35 (30) = happyShift action_44
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (31) = happyShift action_43
action_36 _ = happyFail (happyExpListPerState 36)

action_37 _ = happyReduce_8

action_38 _ = happyReduce_9

action_39 _ = happyReduce_10

action_40 _ = happyReduce_11

action_41 (22) = happyShift action_42
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_6

action_43 _ = happyReduce_24

action_44 (23) = happyShift action_47
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (23) = happyShift action_34
action_45 (12) = happyGoto action_46
action_45 _ = happyReduce_19

action_46 _ = happyReduce_21

action_47 _ = happyReduce_18

happyReduce_1 = happySpecReduce_0  4 happyReduction_1
happyReduction_1  =  HappyAbsSyn4
		 ([]
	)

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Table happy_var_1 happy_var_2
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

happyReduce_5 = happyReduce 4 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (LabeledHeader happy_var_3
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 6 6 happyReduction_6
happyReduction_6 (_ `HappyStk`
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
happyReduction_8 (HappyAbsSyn7  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 _
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (StringType happy_var_1
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 _
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (IntType happy_var_1
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  8 happyReduction_11
happyReduction_11 _
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (BoolType happy_var_1
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_1  9 happyReduction_12
happyReduction_12 (HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_12 _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  9 happyReduction_13
happyReduction_13 (HappyTerminal (Tok _ (TokAlphaNum happy_var_1)))
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_0  10 happyReduction_14
happyReduction_14  =  HappyAbsSyn10
		 ([]
	)

happyReduce_15 = happySpecReduce_2  10 happyReduction_15
happyReduction_15 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  11 happyReduction_16
happyReduction_16 (HappyAbsSyn13  happy_var_2)
	(HappyTerminal (Tok _ (TokAlphaNum happy_var_1)))
	 =  HappyAbsSyn11
		 (Data happy_var_1 happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happyReduce 4 11 happyReduction_17
happyReduction_17 ((HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	(HappyTerminal (Tok _ (TokAlphaNum happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (LabeledData happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 6 11 happyReduction_18
happyReduction_18 ((HappyTerminal (Tok _ (TokString happy_var_6))) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tok _ (TokAlphaNum happy_var_4))) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_2) `HappyStk`
	(HappyTerminal (Tok _ (TokAlphaNum happy_var_1))) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (RelationshipData happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_0  12 happyReduction_19
happyReduction_19  =  HappyAbsSyn12
		 ([]
	)

happyReduce_20 = happySpecReduce_1  12 happyReduction_20
happyReduction_20 (HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn12
		 ([happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  12 happyReduction_21
happyReduction_21 (HappyAbsSyn12  happy_var_3)
	_
	(HappyTerminal (Tok _ (TokString happy_var_1)))
	 =  HappyAbsSyn12
		 (happy_var_1 : happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_0  13 happyReduction_22
happyReduction_22  =  HappyAbsSyn13
		 ([]
	)

happyReduce_23 = happySpecReduce_2  13 happyReduction_23
happyReduction_23 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_23 _ _  = notHappyAtAll 

happyReduce_24 = happyReduce 4 14 happyReduction_24
happyReduction_24 (_ `HappyStk`
	(HappyTerminal (Tok _ (TokString happy_var_3))) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (StringValue happy_var_3
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_2  14 happyReduction_25
happyReduction_25 (HappyTerminal (Tok _ (TokInt happy_var_2)))
	_
	 =  HappyAbsSyn14
		 (IntValue happy_var_2
	)
happyReduction_25 _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_2  14 happyReduction_26
happyReduction_26 (HappyTerminal (Tok _ (TokBool happy_var_2)))
	_
	 =  HappyAbsSyn14
		 (BoolValue happy_var_2
	)
happyReduction_26 _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_2  14 happyReduction_27
happyReduction_27 _
	_
	 =  HappyAbsSyn14
		 (NullValue
	)

happyNewToken action sts stk [] =
	action 32 32 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tok _ TokStringType -> cont 15;
	Tok _ TokIntType -> cont 16;
	Tok _ TokBoolType -> cont 17;
	Tok _ TokId -> cont 18;
	Tok _ TokStartId -> cont 19;
	Tok _ TokEndId -> cont 20;
	Tok _ TokLabel -> cont 21;
	Tok _ TokType -> cont 22;
	Tok _ (TokString happy_dollar_dollar) -> cont 23;
	Tok _ (TokAlphaNum happy_dollar_dollar) -> cont 24;
	Tok _ (TokInt happy_dollar_dollar) -> cont 25;
	Tok _ (TokBool happy_dollar_dollar) -> cont 26;
	Tok _ TokNull -> cont 27;
	Tok _ TokColon -> cont 28;
	Tok _ TokSemiColon -> cont 29;
	Tok _ TokComma -> cont 30;
	Tok _ TokQuoteMark -> cont 31;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 32 tk tks = happyError' (tks, explist)
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
parseError tokens = error $ "Parse error: " ++ tokenPosn (head tokens) ++ "\n" ++ show tokens

type Tables = [Table]

data Table = Table Header [Row]
    deriving (Eq,Show)

type Types = [Type]

data ID = Id String 
        deriving (Eq,Show)

type Relationship = String

type Labels = [String]

data Header = 
    Header Types |
    LabeledHeader Types |
    RelationshipHeader Types
    deriving (Eq,Show)

data Row = 
    Data ID Value |
    LabeledData ID Value Labels |
    RelationshipData ID Value ID Relationship
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
