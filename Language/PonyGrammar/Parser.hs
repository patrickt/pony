{-# OPTIONS_GHC -w #-}
module Language.PonyGrammar.Parser where
import Data.Char

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11
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

action_0 (14) = happyShift action_4
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (14) = happyShift action_2
action_1 _ = happyFail

action_2 (23) = happyShift action_6
action_2 _ = happyFail

action_3 (24) = happyAccept
action_3 _ = happyFail

action_4 (23) = happyShift action_5
action_4 _ = happyFail

action_5 (15) = happyShift action_8
action_5 (19) = happyShift action_7
action_5 _ = happyFail

action_6 (19) = happyShift action_7
action_6 _ = happyFail

action_7 (23) = happyShift action_12
action_7 (5) = happyGoto action_10
action_7 (6) = happyGoto action_11
action_7 _ = happyReduce_4

action_8 (23) = happyShift action_9
action_8 _ = happyFail

action_9 (19) = happyShift action_17
action_9 _ = happyFail

action_10 (20) = happyShift action_16
action_10 _ = happyFail

action_11 (23) = happyShift action_12
action_11 (5) = happyGoto action_15
action_11 (6) = happyGoto action_11
action_11 _ = happyReduce_4

action_12 (12) = happyShift action_13
action_12 (13) = happyShift action_14
action_12 _ = happyFail

action_13 (16) = happyReduce_12
action_13 (17) = happyShift action_24
action_13 (19) = happyShift action_25
action_13 (21) = happyShift action_26
action_13 (23) = happyShift action_27
action_13 (7) = happyGoto action_28
action_13 (8) = happyGoto action_20
action_13 (9) = happyGoto action_21
action_13 (10) = happyGoto action_22
action_13 (11) = happyGoto action_23
action_13 _ = happyReduce_12

action_14 (16) = happyReduce_12
action_14 (17) = happyShift action_24
action_14 (19) = happyShift action_25
action_14 (21) = happyShift action_26
action_14 (23) = happyShift action_27
action_14 (7) = happyGoto action_19
action_14 (8) = happyGoto action_20
action_14 (9) = happyGoto action_21
action_14 (10) = happyGoto action_22
action_14 (11) = happyGoto action_23
action_14 _ = happyReduce_12

action_15 _ = happyReduce_3

action_16 _ = happyReduce_1

action_17 (23) = happyShift action_12
action_17 (5) = happyGoto action_18
action_17 (6) = happyGoto action_11
action_17 _ = happyReduce_4

action_18 (20) = happyShift action_36
action_18 _ = happyFail

action_19 (16) = happyShift action_35
action_19 _ = happyFail

action_20 _ = happyReduce_7

action_21 (16) = happyReduce_12
action_21 (17) = happyShift action_24
action_21 (18) = happyReduce_12
action_21 (19) = happyShift action_25
action_21 (20) = happyReduce_12
action_21 (21) = happyShift action_26
action_21 (22) = happyReduce_12
action_21 (23) = happyShift action_27
action_21 (8) = happyGoto action_34
action_21 (9) = happyGoto action_21
action_21 (10) = happyGoto action_22
action_21 (11) = happyGoto action_23
action_21 _ = happyReduce_12

action_22 _ = happyReduce_10

action_23 (17) = happyShift action_24
action_23 (19) = happyShift action_25
action_23 (21) = happyShift action_26
action_23 (23) = happyShift action_27
action_23 (10) = happyGoto action_33
action_23 (11) = happyGoto action_23
action_23 _ = happyReduce_12

action_24 (17) = happyShift action_24
action_24 (18) = happyReduce_12
action_24 (19) = happyShift action_25
action_24 (21) = happyShift action_26
action_24 (23) = happyShift action_27
action_24 (7) = happyGoto action_32
action_24 (8) = happyGoto action_20
action_24 (9) = happyGoto action_21
action_24 (10) = happyGoto action_22
action_24 (11) = happyGoto action_23
action_24 _ = happyReduce_12

action_25 (17) = happyShift action_24
action_25 (19) = happyShift action_25
action_25 (20) = happyReduce_12
action_25 (21) = happyShift action_26
action_25 (23) = happyShift action_27
action_25 (7) = happyGoto action_31
action_25 (8) = happyGoto action_20
action_25 (9) = happyGoto action_21
action_25 (10) = happyGoto action_22
action_25 (11) = happyGoto action_23
action_25 _ = happyReduce_12

action_26 (17) = happyShift action_24
action_26 (19) = happyShift action_25
action_26 (21) = happyShift action_26
action_26 (22) = happyReduce_12
action_26 (23) = happyShift action_27
action_26 (7) = happyGoto action_30
action_26 (8) = happyGoto action_20
action_26 (9) = happyGoto action_21
action_26 (10) = happyGoto action_22
action_26 (11) = happyGoto action_23
action_26 _ = happyReduce_12

action_27 _ = happyReduce_13

action_28 (16) = happyShift action_29
action_28 _ = happyFail

action_29 _ = happyReduce_6

action_30 (22) = happyShift action_39
action_30 _ = happyFail

action_31 (20) = happyShift action_38
action_31 _ = happyFail

action_32 (18) = happyShift action_37
action_32 _ = happyFail

action_33 _ = happyReduce_11

action_34 _ = happyReduce_8

action_35 _ = happyReduce_5

action_36 _ = happyReduce_2

action_37 _ = happyReduce_14

action_38 _ = happyReduce_16

action_39 _ = happyReduce_15

happyReduce_1 = happyReduce 5 4 happyReduction_1
happyReduction_1 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Grammar happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_2 = happyReduce 7 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn5  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (GrammarAdd happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_0  5 happyReduction_4
happyReduction_4  =  HappyAbsSyn5
		 ([]
	)

happyReduce_5 = happyReduce 4 6 happyReduction_5
happyReduction_5 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (Prod happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 4 6 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn7  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenId happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (ProdAdd happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (Expr happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (happy_var_1 : happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_0  8 happyReduction_9
happyReduction_9  =  HappyAbsSyn8
		 ([]
	)

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Term happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  10 happyReduction_11
happyReduction_11 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1 : happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_0  10 happyReduction_12
happyReduction_12  =  HappyAbsSyn10
		 ([]
	)

happyReduce_13 = happySpecReduce_1  11 happyReduction_13
happyReduction_13 (HappyTerminal (TokenId happy_var_1))
	 =  HappyAbsSyn11
		 (Id happy_var_1
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  11 happyReduction_14
happyReduction_14 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Optional happy_var_2
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  11 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Paren happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  11 happyReduction_16
happyReduction_16 _
	(HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (Many happy_var_2
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 24 24 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenAdd -> cont 12;
	TokenEq -> cont 13;
	TokenGrammar -> cont 14;
	TokenExt -> cont 15;
	TokenPeriod -> cont 16;
	TokenLSBracket -> cont 17;
	TokenRSBracket -> cont 18;
	TokenLBracket -> cont 19;
	TokenRBracket -> cont 20;
	TokenLParen -> cont 21;
	TokenRParen -> cont 22;
	TokenId happy_dollar_dollar -> cont 23;
	_ -> happyError' (tk:tks)
	}

happyError_ 24 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

parseGrammar tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError ts = error ("Parse Error: " ++ (show ts))

data Grammar = Grammar Identifier [Production]
             | GrammarAdd Identifier Identifier [Production]
             deriving Show

data Production = Prod Identifier Expression
                | ProdAdd Identifier Expression
                deriving Show

data Expression = Expr [Term]
                deriving Show
                         
data Term = Term [Factor]
            deriving Show

data Factor = Id Identifier
            | Optional Expression
            | Paren Expression
            | Many Expression
            deriving Show

type Identifier = String

data Token = TokenAdd
           | TokenEq
	   | TokenGrammar
	   | TokenExt
	   | TokenPeriod
	   | TokenLSBracket
	   | TokenRSBracket
	   | TokenLBracket
	   | TokenRBracket
	   | TokenLParen
	   | TokenRParen
	   | TokenId String
           deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
      | isSpace c = lexer cs
      | isAlpha c = lexId (c:cs)
lexer ('.' : cs) = TokenPeriod : (lexer cs)
lexer ('[' : cs) = TokenLSBracket : (lexer cs)
lexer (']' : cs) = TokenRSBracket : (lexer cs)
lexer ('{' : cs) = TokenLBracket : (lexer cs)
lexer ('}' : cs) = TokenRBracket : (lexer cs)
lexer ('(' : cs) = TokenLParen : (lexer cs)
lexer (')' : cs) = TokenRParen : (lexer cs)
lexer (':' : ':' : '=' : cs) = TokenEq : (lexer cs)
lexer (':' : ':' : '+' : cs) = TokenAdd : (lexer cs)

lexId cs = case span isAlphaNum cs of
  ("Grammar", rest) -> TokenGrammar : (lexer rest)
  ("extends", rest) -> TokenExt : (lexer rest)
  (id, rest) -> (TokenId id) : (lexer rest)

parseGrammar' :: String -> Grammar
parseGrammar' = parseGrammar . lexer
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

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

{-# LINE 148 "templates/GenericTemplate.hs" #-}

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
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
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
