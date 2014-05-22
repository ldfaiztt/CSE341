{-# OPTIONS_GHC -w #-}
{- CSE 341, Autumn 2012.  Definition of a parser for polynomials for
use in the polynomial multiplier.  The only function you'll need from
this module is   string_to_raw_poly, which has the following type:

  string_to_raw_poly :: String -> (String,[(Integer,Integer)])

This function takes a string representing a polynomial, parses and
normalizes it, and returns a representation of the raw polynomial as a
String (the variable) and a list of coefficient-exponent pairs.  These
are sorted by exponent, largest first.  Terms with a 0 coefficient are
dropped from the list, and any terms with the same exponent will be
combined. -}


module PolyParser (string_to_raw_poly)
    where
import Data.Char

-- parser produced by Happy Version 1.18.9

data HappyAbsSyn t4 t5 t6
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6

action_0 (7) = happyShift action_5
action_0 (8) = happyShift action_6
action_0 (9) = happyShift action_7
action_0 (10) = happyShift action_8
action_0 (4) = happyGoto action_9
action_0 (5) = happyGoto action_3
action_0 (6) = happyGoto action_4
action_0 _ = happyFail

action_1 (7) = happyShift action_5
action_1 (8) = happyShift action_6
action_1 (9) = happyShift action_7
action_1 (10) = happyShift action_8
action_1 (4) = happyGoto action_2
action_1 (5) = happyGoto action_3
action_1 (6) = happyGoto action_4
action_1 _ = happyFail

action_2 (9) = happyShift action_10
action_2 (10) = happyShift action_11
action_2 _ = happyFail

action_3 _ = happyReduce_5

action_4 _ = happyReduce_7

action_5 (11) = happyShift action_15
action_5 _ = happyReduce_10

action_6 (12) = happyShift action_14
action_6 _ = happyReduce_8

action_7 (7) = happyShift action_5
action_7 (8) = happyShift action_6
action_7 (5) = happyGoto action_13
action_7 (6) = happyGoto action_4
action_7 _ = happyFail

action_8 (7) = happyShift action_5
action_8 (8) = happyShift action_6
action_8 (5) = happyGoto action_12
action_8 (6) = happyGoto action_4
action_8 _ = happyFail

action_9 (9) = happyShift action_10
action_9 (10) = happyShift action_11
action_9 (13) = happyAccept
action_9 _ = happyFail

action_10 (7) = happyShift action_5
action_10 (8) = happyShift action_6
action_10 (5) = happyGoto action_20
action_10 (6) = happyGoto action_4
action_10 _ = happyFail

action_11 (7) = happyShift action_5
action_11 (8) = happyShift action_6
action_11 (5) = happyGoto action_19
action_11 (6) = happyGoto action_4
action_11 _ = happyFail

action_12 _ = happyReduce_4

action_13 _ = happyReduce_3

action_14 (7) = happyShift action_18
action_14 _ = happyFail

action_15 (7) = happyShift action_17
action_15 (8) = happyShift action_6
action_15 (6) = happyGoto action_16
action_15 _ = happyFail

action_16 _ = happyReduce_6

action_17 _ = happyReduce_10

action_18 _ = happyReduce_9

action_19 _ = happyReduce_2

action_20 _ = happyReduce_1

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Plus happy_var_1 happy_var_3
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_3  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_2 _ _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  4 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (UnaryPlus happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  4 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (UnaryMinus happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  4 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (BasicTerm happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_3)
	_
	(HappyTerminal (TokenInteger happy_var_1))
	 =  HappyAbsSyn5
		 (Times happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (Primary happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  6 happyReduction_8
happyReduction_8 (HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn6
		 (Var happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  6 happyReduction_9
happyReduction_9 (HappyTerminal (TokenInteger happy_var_3))
	_
	(HappyTerminal (TokenVar happy_var_1))
	 =  HappyAbsSyn6
		 (Power happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  6 happyReduction_10
happyReduction_10 (HappyTerminal (TokenInteger happy_var_1))
	 =  HappyAbsSyn6
		 (Integer happy_var_1
	)
happyReduction_10 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 13 13 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInteger happy_dollar_dollar -> cont 7;
	TokenVar happy_dollar_dollar -> cont 8;
	TokenPlus -> cont 9;
	TokenMinus -> cont 10;
	TokenTimes -> cont 11;
	TokenPower -> cont 12;
	_ -> happyError' (tk:tks)
	}

happyError_ 13 tk tks = happyError' tks
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

polyparse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

-- declarations of datatypes for the parser result

data Exp  
      = Plus Exp Term 
      | Minus Exp Term 
      | UnaryPlus Term
      | UnaryMinus Term
      | BasicTerm Term
      deriving Show

data Term 
      = Times Integer Primary
      | Primary Primary
      deriving Show

data Primary
      = Var String
      | Power String Integer
      | Integer Integer
      deriving Show

data Token
      = TokenInteger Integer
      | TokenVar String
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenPower
      deriving Show

-- a lexer to take the input string and break it into a list of tokens

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenTimes : lexer cs
lexer ('^':cs) = TokenPower : lexer cs

lexNum cs = TokenInteger (read num) : lexer rest
       where (num,rest) = span isDigit cs

lexVar cs = TokenVar var : lexer rest
       where (var,rest) = span isAlpha cs

-- some convenient type synonyms
type RawTerm = (Integer,Integer)
type RawPoly = (String,[RawTerm])

-- convert an exp to a raw poly (normalized)
exp_to_raw_poly :: Exp -> RawPoly
exp_to_raw_poly e = (get_expr_var e, normalize (exp_to_raw_terms e))

-- helper function to extract the variable from an expression; also
-- check for two or more different variables; if found give an error
get_expr_var :: Exp -> String
get_expr_var (Plus e t) = same_vars (get_expr_var e) (get_term_var t)
get_expr_var (Minus e t) = same_vars (get_expr_var e) (get_term_var t)
get_expr_var (UnaryPlus t) = get_term_var t
get_expr_var (UnaryMinus t) = get_term_var t
get_expr_var (BasicTerm t) = get_term_var t

get_term_var :: Term -> String
get_term_var (Times c (Var s)) = s
get_term_var (Times c (Power s e)) = s
get_term_var (Primary (Var s)) = s
get_term_var (Primary (Power s e)) =  s
get_term_var (Primary (Integer c)) = ""

same_vars "" v = v
same_vars v "" = v
same_vars v w = if v==w then v else error "more than one variable in polynomial"

-- turn an exp into a list of terms (not normalized)
exp_to_raw_terms :: Exp -> [RawTerm]
exp_to_raw_terms (Plus e t) = exp_to_raw_terms e ++ [term_to_raw_term t]
exp_to_raw_terms (Minus e t) = exp_to_raw_terms e ++ [(negate_term (term_to_raw_term t))]
exp_to_raw_terms (UnaryPlus t) = [term_to_raw_term t]
exp_to_raw_terms (UnaryMinus t) = [negate_term (term_to_raw_term t)]
exp_to_raw_terms (BasicTerm t) = [term_to_raw_term t]

-- helper function to convert a term to a raw term in normal form (with explicit coefficient and exponent)
term_to_raw_term :: Term -> RawTerm
term_to_raw_term (Times c (Var s)) = (c,1)
term_to_raw_term (Times c (Power s e)) = (c,e)
term_to_raw_term (Primary (Var s)) = (1,1)
term_to_raw_term (Primary (Power s e)) =  (1,e)
term_to_raw_term (Primary (Integer c)) = (c,0)

-- negate a raw term
negate_term (c,e) = (-c,e)

-- normalize a raw poly (i.e. sort the terms, drop ones with 0 coefficient)
normalize :: [RawTerm] -> [RawTerm]
normalize [] = []
normalize ((0,e) : ts) = normalize ts
normalize ((c,e) : ts) = insert (c,e) (normalize ts)

insert (c,e) [] = [(c,e)]
insert (c1,e1) ((c2,e2) : ts) 
    | c1==0 = ((c2,e2) : ts)
    | e1>e2 = (c1,e1) : (c2,e2) : ts
    | e1==e2 && c1+c2==0 = ts
    | e1==e2 = (c1+c2,e1) : ts
    | e1<e2 = (c2,e2) : (insert (c1,e1) ts)

{- the funtion of interest in this module -- see the comment at the
top.  Converts a string into a raw polynomial. -}
string_to_raw_poly :: String -> RawPoly
string_to_raw_poly = exp_to_raw_poly . polyparse .lexer
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
