{

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
}

%name polyparse
%tokentype { Token }
%error { parseError }

%token 
      integer         { TokenInteger $$ }
      var             { TokenVar $$ }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '^'             { TokenPower }
%%
      
Exp  : Exp '+' Term             { Plus $1 $3 }
     | Exp '-' Term             { Minus $1 $3 }
     | '+' Term                 { UnaryPlus $2 }
     | '-' Term                 { UnaryMinus $2 }
     | Term                     { BasicTerm $1 }

Term  : integer '*' Primary     { Times $1 $3 }
      | Primary                 { Primary $1 }

Primary  : var                  { Var $1 }
         | var '^' integer      { Power $1 $3 }
         | integer              { Integer $1 }

{
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


}
